package vct.col.newrewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.{FuncTools, ScopedStack}
import vct.col.ast._
import vct.col.newrewrite.util.FreeVariables
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite._
import vct.result.VerificationError.{Unreachable, UserError}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

case object ApplyTermRewriter {
  case class BuilderFor[Rule]
  (
    ruleNodes: Seq[SimplificationRule[Rule]],
    debugIn: Seq[String],
    debugMatch: Boolean,
    debugMatchShort: Boolean,
    debugNoMatch: Boolean,
    debugFilterInputKind: Option[String],
    debugFilterRule: Option[String],
  ) extends RewriterBuilder {
    override def apply[Pre <: Generation](): Rewriter[Pre] =
      ApplyTermRewriter(ruleNodes, debugIn, debugMatch, debugMatchShort, debugNoMatch, debugFilterInputKind, debugFilterRule)
    override def key: String = "simplify"
    override def desc: String = "Apply axiomatic simplification lemmas to expressions."
  }
}

case class ApplyTermRewriter[Rule, Pre <: Generation]
(
  ruleNodes: Seq[SimplificationRule[Rule]],
  debugIn: Seq[String],
  debugMatch: Boolean,
  debugMatchShort: Boolean,
  debugNoMatch: Boolean,
  debugFilterInputKind: Option[String],
  debugFilterRule: Option[String],
) extends Rewriter[Pre] with LazyLogging {
  case class MalformedSimplificationRule(body: Expr[_]) extends UserError {
    override def code: String = "malformedSimpRule"
    override def text: String =
      body.o.messageInContext(
        "The body of a simplification rule must be any number of nested \\forall predicates, " +
          "of which the body is an equality.")
  }

  val rules: Seq[(Seq[Variable[Rule]], Expr[Rule], Expr[Rule], Origin)] = ruleNodes.map(node => (node.o, consumeForalls(node.axiom))).map {
    case (o, (free, body)) => body match {
      case Eq(left, right) => (free, left, right, o)
      case other => throw MalformedSimplificationRule(other)
    }
  }

  val ruleMap: Map[java.lang.Class[_], Seq[(Seq[Variable[Rule]], Expr[Rule], Expr[Rule], Origin)]] =
    rules.groupBy {
      case (_, pattern, _, _) => pattern.getClass
    }

  val debugNameStack: ScopedStack[String] = ScopedStack()

  def consumeForalls(node: Expr[Rule]): (Seq[Variable[Rule]], Expr[Rule]) = node match {
    case Forall(bindings, _, body) =>
      val (innerBindings, innerBody) = consumeForalls(body)
      (bindings ++ innerBindings, innerBody)
    case other => (Nil, other)
  }

  case class ApplyParametricBindings(bindings: Map[Variable[Pre], Ref[Pre, Variable[Pre]]]) extends NonLatchingRewriter[Pre, Pre] {
    override def succ[DPost <: Declaration[Pre]](decl: Declaration[Pre])(implicit tag: ClassTag[DPost]): Ref[Pre, DPost] = decl match {
      case v: Variable[Pre] => bindings.getOrElse(v, new LazyRef[Pre, DPost](v)).asInstanceOf[Ref[Pre, DPost]]
      case other => new LazyRef(other)
    }

    override def dispatch(decl: Declaration[Pre]): Unit = decl.succeedDefault(decl)
  }

  case class ApplyRule(inst: Map[Variable[Rule], (Expr[Pre], Seq[Variable[Pre]])], typeInst: Map[Variable[Rule], Type[Pre]], defaultOrigin: Origin) extends NonLatchingRewriter[Rule, Pre] {
    override def dispatch(o: Origin): Origin = defaultOrigin

    override def dispatch(e: Expr[Rule]): Expr[Pre] = e match {
      case Local(Ref(v)) =>
        if(inst.contains(v)) ApplyParametricBindings(Map.empty).dispatch(inst(v)._1)
        else Local[Pre](succ(v))(e.o)
      case FunctionOf(Ref(v), ruleVars) =>
        val (replacement, vars) = inst(v)
        ApplyParametricBindings(vars.zip(ruleVars.map(succ[Variable[Pre]])).toMap).dispatch(replacement)
      case other => rewriteDefault(other)
    }

    override def dispatch(t: Type[Rule]): Type[Pre] = t match {
      case TVar(Ref(v)) => typeInst(v) // PB: maybe this is wrong in contrived situations?
      case other => rewriteDefault(other)
    }
  }

  def apply(rule: (Seq[Variable[Rule]], Expr[Rule], Expr[Rule], Origin), subject: Expr[Pre]): Option[Expr[Pre]] = {
    incApply()
    implicit val o: Origin = DiagnosticOrigin
    val (free, pattern, subtitute, ruleOrigin) = rule

    val debugFilter =
      debugFilterRule.map(_ == ruleOrigin.preferredName).getOrElse(true) &&
        debugFilterInputKind.map(_ == subject.getClass.getSimpleName).getOrElse(true) &&
        (debugIn.isEmpty || debugIn.exists(name => debugNameStack.exists(_ == name)))

    val inst = mutable.Map[Variable[Rule], (Expr[Pre], Seq[Variable[Pre]])]()
    val typeInst = mutable.Map[Variable[Rule], Type[Pre]]()
    val bindingInst = mutable.Map[Variable[Rule], Variable[Pre]]()

    lazy val debugHeader: String = s"Expression `$subject` does not match rule ${ruleOrigin.preferredName}, since"

    def declareTypeInst(left: Variable[Rule], right: Type[Pre]): Boolean =
      typeInst.get(left) match {
        case Some(replacement) =>
          val matches = replacement == right
          if(debugNoMatch && debugFilter && !matches)
            logger.debug(s"$debugHeader earlier `$left` matched `$replacement`, but now it must match `$right`")

          matches
        case None =>
          typeInst(left) = right
          val leftUpperBound = left.t.asInstanceOf[TType[Rule]].t
          val matches = leftUpperBound.superTypeOf(right.asInstanceOf[Type[Rule]])

          if(debugNoMatch && debugFilter && !matches) {
            logger.debug(s"$debugHeader the type-level variable `$left` matched `$right`, but " +
              s"the upper bound of `$left` (`$leftUpperBound`) is not a supertype of $right.")
          }

          matches
      }

    def declareInst(left: Variable[Rule], right: Expr[Pre], leftBindings: Seq[Variable[Rule]]): Boolean = {
      lazy val debugLeft = Local[Rule](left.ref)
      inst.get(left) match {
        case Some((replacement, _)) =>
          val matches = replacement == right

          if(debugNoMatch && debugFilter && !matches)
            logger.debug(s"$debugHeader earlier `$debugLeft` matched `$replacement`, but now it must match `$right`")

          matches
        case None =>
          val freeRight = FreeVariables.freeVariables(right).collect {
            case FreeVariables.FreeVar(Local(Ref(v))) => v
          }.toSet

          val freeRightOfBindings = freeRight.intersect(bindingInst.values.toSet)
          val declaredAllowedBindings = leftBindings.map(bindingInst).toSet
          val extraBindingDeps = freeRightOfBindings -- declaredAllowedBindings

          if(extraBindingDeps.nonEmpty) {
            if(debugNoMatch && debugFilter)
              logger.debug(s"$debugHeader `$debugLeft` matches `$right`, but it is dependent on bindings within the pattern that are not declared as such: ${extraBindingDeps.map(v => Local[Pre](v.ref).toString).mkString(", ")}.")

            return false
          }

          inst(left) = (right, leftBindings.map(bindingInst))
          val matches = left.t.superTypeOf(right.t.asInstanceOf[Type[Rule]])

          if(debugNoMatch && debugFilter && !matches)
            logger.debug(s"$debugHeader `$debugLeft` (typed `${left.t}`) matched `$right` (typed `${right.t}`), " +
              s"but `${right.t}` is not a subtype of `${left.t}`")

          matches
      }
    }

    Comparator.compare(pattern, subject).foreach {
      case Comparator.MatchingDeclaration(left: Variable[Rule], right: Variable[Pre]) =>
        bindingInst(left) = right
      case Comparator.MatchingDeclaration(_, _) =>
        throw Unreachable("Simplification rules do not declare anything other than variables from binders.")

      case Comparator.MatchingReference(left: Variable[Rule], right: Variable[Pre]) =>
        if(free.contains(left)) {
          if(!(left.t match {
            case TType(_) => declareTypeInst(left, TVar(right.ref))
            case _ => declareInst(left, Local(right.ref), Nil)
          })) {
            return None
          }
        } else /* !free.contains(left) */ {
          if(!bindingInst.get(left).contains(right)) {
            return None
          }
        }
      case Comparator.MatchingReference(_, _) =>
        throw Unreachable("Simplification rules do not refer to anything other than variables.")

      case Comparator.StructuralDifference(Local(Ref(v)), right: Expr[Pre]) if free.contains(v) =>
        if(!declareInst(v, right, Nil)) return None

      case Comparator.StructuralDifference(FunctionOf(Ref(v), bindings), right: Expr[Pre]) if free.contains(v) =>
        if(!declareInst(v, right, bindings.map(_.decl))) return None

      case Comparator.StructuralDifference(TVar(Ref(v)), right: Type[Pre]) if free.contains(v) =>
        if(!declareTypeInst(v, right)) return None

      case Comparator.StructuralDifference(left, right) =>
        if(debugNoMatch && debugFilter)
          logger.debug(s"$debugHeader $left cannot be matched to $right.")
        return None
    }

    val result = ApplyRule(inst.toMap, typeInst.toMap, subject.o).dispatch(subtitute)

    if(debugMatch && debugFilter) {
      if(debugMatchShort) {
        logger.debug(subject.toString)
        logger.debug(s" ~> $result")
      } else {
        logger.debug(s"Expression:       $subject")
        logger.debug(s"Matches:          $pattern")
        if (inst.nonEmpty) {
          logger.debug("With bindings:")
          inst.toSeq.sortBy { case (k, _) => k.o.preferredName }.foreach {
            case (rule, (binding, over)) =>
              if (over.isEmpty)
                logger.debug(s"  $rule = $binding")
              else
                logger.debug(s"  $rule = ($binding) parametric over ${over.mkString(", ")}")
          }
        }
        if (typeInst.nonEmpty) {
          logger.debug("With type bindings:")
          typeInst.foreach {
            case (rule, binding) => logger.debug(s"  $rule = $binding")
          }
        }
        logger.debug(s"Applied to:       $subtitute")
        logger.debug(s"Result:           $result")
        logger.debug("")
      }
    }

    incSuccess()
    Some(result)
  }

  def applyOnce(expr: Expr[Pre]): Option[Expr[Pre]] =
    FuncTools.firstOption(ruleMap.getOrElse(expr.getClass, Nil), apply(_, expr))

  @tailrec
  final def applyExhaustively(expr: Expr[Pre]): Expr[Pre] =
    applyOnce(expr) match {
      case Some(e) =>
        applyExhaustively(e)
      case None => expr
    }

  case class ApplyRecursively() extends NonLatchingRewriter[Pre, Pre] {
    override def succ[DPost <: Declaration[Pre]](decl: Declaration[Pre])(implicit tag: ClassTag[DPost]): Ref[Pre, DPost] =
      new LazyRef(decl)

    @tailrec
    override final def dispatch(e: Expr[Pre]): Expr[Pre] = {
      val simplifiedTopDown = applyExhaustively(e)
      val simplifiedBottomUp = rewriteDefault(simplifiedTopDown)

      applyOnce(simplifiedBottomUp) match {
        case Some(simplifiedYetAgain) =>
          // The simplification of the child nodes caused the parent node to be simplified again, so we need to
          // recurse into the structure once again.
          logger.debug(s"Complicated simplification: simplified node $simplifiedBottomUp was simplified yet again to $simplifiedYetAgain")
          dispatch(simplifiedYetAgain)
        case None => simplifiedBottomUp
      }
    }

    override def dispatch(decl: Declaration[Pre]): Unit = decl.succeedDefault(decl)
  }

  val simplificationDone: ScopedStack[Unit] = ScopedStack()
  var countApply: Int = 0
  var countSuccess: Int = 0
  var currentExpr: Expr[Pre] = _

  def incApply(): Unit = {
    countApply += 1

    if(countApply % 10000 == 0) {
      logger.debug(s"Applied $countApply rules ($countSuccess successfully) to expression $currentExpr")
    }
  }

  def incSuccess(): Unit = {
    countSuccess += 1
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    if(simplificationDone.nonEmpty) rewriteDefault(e)
    else simplificationDone.having(()) {
      countApply = 0
      countSuccess = 0
      currentExpr = e
      dispatch(ApplyRecursively().dispatch(e))
    }

  override def dispatch(decl: Declaration[Pre]): Unit =
    debugNameStack.having(decl.o.preferredName) {
      rewriteDefault(decl)
    }
}
