package vct.col.newrewrite

import hre.util.FuncTools
import vct.col.ast._
import vct.col.newrewrite.util.Substitute
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.VarBuildHelpers
import vct.result.VerificationResult.{Unreachable, UserError}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

case object ApplyTermRewriter {
  case class BuilderFor[Rule](ruleNodes: Seq[SimplificationRule[Rule]]) extends RewriterBuilder {
    override def apply[Pre <: Generation](): Rewriter[Pre] = ApplyTermRewriter(ruleNodes)
  }
}

case class ApplyTermRewriter[Rule, Pre <: Generation](ruleNodes: Seq[SimplificationRule[Rule]]) extends Rewriter[Pre] {
  case class MalformedSimplificationRule(body: Expr[_]) extends UserError {
    override def code: String = "malformedSimpRule"
    override def text: String =
      body.o.messageInContext(
        "The body of a simplification rule must be any number of nested \\forall predicates, " +
          "of which the body is an equality.")
  }

  val rules: Seq[(Seq[Variable[Rule]], Expr[Rule], Expr[Rule])] = ruleNodes.map(node => consumeForalls(node.axiom)).map {
    case (free, body) => body match {
      case Eq(left, right) => (free, left, right)
      case other => throw MalformedSimplificationRule(other)
    }
  }

  def consumeForalls(node: Expr[Rule]): (Seq[Variable[Rule]], Expr[Rule]) = node match {
    case Forall(bindings, _, body) =>
      val (innerBindings, innerBody) = consumeForalls(body)
      (bindings ++ innerBindings, innerBody)
    case other => (Nil, other)
  }

  case class CopyRule() extends NonLatchingRewriter[Rule, Pre] {
    override def succ[DPost <: Declaration[Pre]](decl: Declaration[Rule])(implicit tag: ClassTag[DPost]): Ref[Pre, DPost] =
      new LazyRef[Pre, DPost](successionMap.get(decl).getOrElse(decl.asInstanceOf[Declaration[Pre]]))
  }

  def apply(rule: (Seq[Variable[Rule]], Expr[Rule], Expr[Rule]), subject: Expr[Pre]): Option[Expr[Pre]] = {
    val debugNoMatch = true
    val debugMatch = true
    val debugRawDifferences = true

    val (free, pattern, subtitute) = rule
    val inst = mutable.Map[Expr[Rule], Expr[Pre]]()
    val typeInst = mutable.Map[TVar[Rule], Type[Pre]]()
    val bindingInst = mutable.Map[Variable[Rule], Variable[Pre]]()

    implicit val o: Origin = DiagnosticOrigin

    val diffs: Iterable[Comparator.Difference[Rule, Pre]] =
      if(debugRawDifferences) {
        val computedDiffs = Comparator.compare(pattern, subject).toIndexedSeq
        println(s"Raw diffs: $computedDiffs")
        computedDiffs
      } else {
        // lazy; exits comparison on first irreconcilable difference.
        Comparator.compare(pattern, subject)
      }

    lazy val debugHeader: String = s"Expression `$subject` does not match rule `$pattern`, since"

    diffs.foreach {
      case Comparator.MatchingDeclaration(left: Variable[Rule], right: Variable[Pre]) =>
        bindingInst(left) = right
        /* If we match two declared variables, we simply insert them into inst/typeInst. The check whether the declaration
         * is already in the map is purely for defensive programming: a reference to a declaration cannot occur in the
         * comparison stream before it is declared. We do not need to check whether the types of the declaration are
         * compatible, because the type in the declaration is also compared: we will get a separate
         * Comparator.StructuralDifference for the type. */
        left.t match {
          case TType(_) =>
            if(typeInst.getOrElseUpdate(TVar(left.ref), TVar(right.ref)) != TVar[Pre](right.ref)) {
              if(debugNoMatch)
                println(s"$debugHeader earlier `${left.get}` matched `${inst(left.get)}`, but now it must match `${right.get}`")
              return None
            }
          case _ =>
            if(inst.getOrElseUpdate(left.get, right.get) != right.get) {
              if(debugNoMatch)
                println(s"$debugHeader earlier `${left.get}` matched `${inst(left.get)}`, but now it must match `${right.get}`")
              return None
            }
        }
      case Comparator.MatchingDeclaration(left, right) =>
        throw Unreachable("Simplification rules do not declare anything other than variables from binders.")

      case Comparator.MatchingReference(left: Variable[Rule], right: Variable[Pre]) =>
        if(!free.contains(left) && !inst.contains(left.get) && !typeInst.contains(TVar(left.ref))) {
          throw Unreachable("Variables that are not free must first be seen as a declaration.")
        }

        left.t match {
          case TType(upperTypeBound) =>
            typeInst.get(TVar(left.ref)) match {
              case Some(replacement) =>
                /* I'm pretty sure we have no rewrite rules that have type-level variables that are not free, so we
                 * probably just have seen this type variable before. */

                if(replacement != TVar[Pre](right.ref)) {
                  if(debugNoMatch)
                    println(s"$debugHeader earlier `${left.get}` matched `$replacement`, but now it must match `${right.get}`")
                  return None
                }
              case None =>
                val rightUpperBound = right.t.asInstanceOf[TType[Pre]].t.asInstanceOf[Type[Rule]]
                if(!upperTypeBound.superTypeOf(rightUpperBound)) {
                  if(debugNoMatch)
                    println(s"$debugHeader the type-level variable `${left.get}` matched `${right.get}`, but " +
                      s"the upper bound of `${left.get}` (`$upperTypeBound`) is not a supertype of " +
                      s"the upper bound of `${right.get}`: `$rightUpperBound`")

                  return None
                } else {
                  typeInst(TVar(left.ref)) = TVar(right.ref)
                }
            }
          case _ =>
            inst.get(left.get) match {
              case Some(replacement) =>
                /* The variable is not free or has been encountered earlier. This means the type-check has already
                 * occurred and we can lean on the fact that the right expression must now be identical. */
                if(replacement != right.get) {
                  if(debugNoMatch)
                    println(s"$debugHeader earlier `${left.get}` matched `$replacement`, but now it must match `${right.get}`")
                  return None
                }
              case None =>
                /* The variable is free and has not been encountered yet. We have to put it in inst and check that the
                 * type of the replacement is a subtype of the declared type of the free variable. */

                /* SAFETY: we can coerce types for superTypeOf, since coercion computation is cross-generation safe. The
                 * only effect is that types that depend on declaration (TClass(_) etc.) will not match, but the
                 * simplification rules do not declare such types, so they would never match anyway. */
                if(!left.t.superTypeOf(right.t.asInstanceOf[Type[Rule]])) {
                  if(debugNoMatch)
                    println(s"$debugHeader `${left.get}` (typed `${left.t}`) matched `${right.get}` (typed `${right.t}`), " +
                      s"but `${right.t}` is not a subtype of `${left.t}`")
                  return None
                } else {
                  inst(left.get) = right.get
                }
            }
        }
      case Comparator.MatchingReference(left, right) =>
        throw Unreachable("Simplification rules do not refer to anything other than variables.")

      case Comparator.StructuralDifference(left @ Local(Ref(v)), right: Expr[Pre]) if free.contains(v) =>
        inst.get(left) match {
          case Some(replacement) =>
            if(replacement != right) {
              if(debugNoMatch)
                println(s"$debugHeader earlier `$left` matched `$replacement`, but now it must match `$right`")
              return None
            }
          case None =>
            if(!left.t.superTypeOf(right.t.asInstanceOf[Type[Rule]])) {
              if(debugNoMatch)
                println(s"$debugHeader `$left` (typed `${left.t}`) matched `$right` (typed `${right.t}`), " +
                  s"but `${right.t}` is not a subtype of `${left.t}`")
              return None
            } else {
              inst(left) = right
            }
        }

      case Comparator.StructuralDifference(left @ TVar(Ref(v)), right: Type[Pre]) if free.contains(v) =>
        typeInst.get(TVar(left.ref)) match {
          case Some(replacement) =>
            if(replacement != right) {
              if(debugNoMatch)
                println(s"$debugHeader earlier `$left` matched `$replacement`, but now it must match `$right`")
              return None
            }
          case None =>
            val leftUpperBound = v.t.asInstanceOf[TType[Rule]].t
            if(!leftUpperBound.superTypeOf(right.asInstanceOf[Type[Rule]])) {
              if(debugNoMatch)
                println(s"$debugHeader the type-level variable `$left` matched `$right`, but " +
                  s"the upper bound of `$left` (`$leftUpperBound`) is not a supertype of $right.")
              return None
            } else {
              typeInst(left) = right
            }
        }

      case Comparator.StructuralDifference(left, right) =>
        if(debugNoMatch)
          println(s"$debugHeader $left cannot be matched to $right.")
        return None
    }

    // Free declarations are transmuted from Declaration[Rule] -> Declaration[Pre] when copying, so we can safely transmute
    // the inst keys. It would be a bit cleaner to make fresh [Pre] copies of left and right to avoid these issues,
    // but that is much less performant.

    val copier = CopyRule()
    val freshSubtitute = copier.dispatch(subtitute)
    val preInst = inst.map { case (k, v) => k.asInstanceOf[Expr[Pre]] -> v }.toMap
    val preTypeInst = typeInst.map { case (k, v) => k.asInstanceOf[TVar[Pre]] -> v }.toMap
    val preBindingInst = bindingInst.map { case (k, v) => copier.succ[Variable[Pre]](k).decl -> v }.toMap
    val result = Substitute(preInst, preTypeInst, preBindingInst).dispatch(freshSubtitute)

    if(debugMatch) {
      println(s"Expression:       $subject")
      println(s"Matches:          $pattern")
      if(inst.nonEmpty) {
        println("With bindings:")
        inst.foreach {
          case (rule, binding) => println(s"  $rule = $binding")
        }
      }
      if(typeInst.nonEmpty) {
        println("With type bindings:")
        typeInst.foreach {
          case (rule, binding) => println(s"  $rule = $binding")
        }
      }
      println(s"Applied to:       $subtitute")
      println(s"Result:           $result")
      println()
    }

    Some(result)
  }

  @tailrec
  final def applyExhaustively(expr: Expr[Pre]): Expr[Pre] =
    FuncTools.firstOption(rules, apply(_, expr)) match {
      case Some(e) =>
        applyExhaustively(e)
      case None => expr
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    rewriteDefault(applyExhaustively(e))
}
