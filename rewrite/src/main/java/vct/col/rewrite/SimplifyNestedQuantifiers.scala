package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{ArraySubscript, _}
import vct.col.ast.util.{AnnotationVariableInfoGetter, ExpressionEqualityCheck}
import vct.col.rewrite.util.Comparison
import vct.col.origin.{ArrayInsufficientPermission, Origin, PanicBlame, PointerBounds}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, Substitute}
import vct.result.VerificationError.Unreachable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.annotation.nowarn

/**
  * This rewrite pass simplifies expressions of roughly this form:
  *   forall(i,j: Int . 0 <= i < i_max && 0 <= j < j_max;  xs[a*(i_max*j + i) + b])
  * and collapses it into a single forall:
  *   forall(k: Int. b <= k <= i_max*j_max*a + b && k % a == 0; xs[k])
  *
  * We also check on if a quantifier takes just a single value. E.g.
  *   forall(i,j: Int; i == 5 && i < n && i <= j && j < 5; xs[j+i]) ====> 5 < n ==> forall(int j; 0 <= j < 5; xs[j])
  *
  * and if a quantifier isn't in the "body" of the forall. E.g.
  *   forall(i,j: Int. 1 <= i && i< n && 0 < j; xs[j]>0) ====> n > 1 ==> forall(j: Int; 0 < j; xs[j] >0)
  *
  */
case object SimplifyNestedQuantifiers extends RewriterBuilder {
  override def key: String = "simplifyNestedQuantifiers"
  override def desc: String = "Simplify nested quantifiers."
}

case class SimplifyNestedQuantifiers[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {

  case object SimplifyNestedQuantifiersOrigin extends Origin {
    override def preferredName: String = "unknown"

    override def shortPosition: String = "generated"

    override def context: String = "[At generated expression for the simplification of nested quantifiers]"

    override def inlineContext: String = "[Simplified expression]"
  }

  case class BinderOrigin(name: String) extends Origin {
    override def preferredName: String = name

    override def shortPosition: String = "generated"

    override def context: String = "[At generated expression for the simplification of nested quantifiers]"

    override def inlineContext: String = "[Simplified expression]"
  }

  private implicit val o: Origin = SimplifyNestedQuantifiersOrigin

  private def one: IntegerValue[Pre] = IntegerValue(1)

  var equalityChecker: ExpressionEqualityCheck[Pre] = ExpressionEqualityCheck()

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case e: Binder[Pre] =>
        rewriteLinearArray(e) match {
          case None =>
            val res = rewriteDefault(e)
            res match {
              case Starall(_, Nil, body) if !body.exists { case InlinePattern(_, _, _) => true } =>
                logger.warn(f"The binder `$e` contains no triggers")
              case Forall(_, Nil, body) if !body.exists { case InlinePattern(_, _, _) => true } =>
                logger.warn(f"The binder `$e` contains no triggers")
              case _ =>
            }
            res
          case Some(newE)
            => newE
        }
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    val e = stat match {
      case Exhale(e) => e
      case Inhale(e) => e
      case _ => return rewriteDefault(stat)
    }

    val conditions = getConditions(e)
    val infoGetter = new AnnotationVariableInfoGetter[Pre]()
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.getInfo(conditions)))
    val result = rewriteDefault(stat)
    equalityChecker = ExpressionEqualityCheck()
    result
  }

  def getConditions(preds: AccountedPredicate[Pre]): Seq[Expr[Pre]] = preds match {
    case UnitAccountedPredicate(pred) => getConditions(pred)
    case SplitAccountedPredicate(left, right) => getConditions(left) ++ getConditions(right)
  }

  def getConditions(e: Expr[Pre]): Seq[Expr[Pre]] = e match {
    case And(left, right) => getConditions(left) ++ getConditions(right)
    case Star(left, right) => getConditions(left) ++ getConditions(right)
    case other => Seq[Expr[Pre]](other)
  }

  override def dispatch(loopContract: LoopContract[Pre]): LoopContract[Post] = {
    val loopInvariant: LoopInvariant[Pre] = loopContract match {
      case l: LoopInvariant[Pre] => l
      case _ => return dispatch(loopContract)
    }

    val infoGetter = new AnnotationVariableInfoGetter[Pre]()
    val conditions = getConditions(loopInvariant.invariant)
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.getInfo(conditions)))
    val invariant = dispatch(loopInvariant.invariant)
    equalityChecker = ExpressionEqualityCheck()

    LoopInvariant(invariant)(loopInvariant.o)
  }

  override def dispatch(contract: ApplicableContract[Pre]): ApplicableContract[Post] = {
    val infoGetter = new AnnotationVariableInfoGetter[Pre]()
    val reqConditions = getConditions(contract.requires)
    val contextConditions = getConditions(contract.contextEverywhere)
    val ensureConditions = getConditions(contract.ensures)
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.getInfo(reqConditions ++ contextConditions)))
    val requires = dispatch(contract.requires)
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.getInfo(ensureConditions ++ contextConditions)))
    val ensures = dispatch(contract.ensures)
    equalityChecker = ExpressionEqualityCheck(Some(infoGetter.getInfo(contextConditions)))
    val contextEverywhere = dispatch(contract.contextEverywhere)
    equalityChecker = ExpressionEqualityCheck()

    val signals = contract.signals.map(element => dispatch(element))
    val givenArgs = variables.collect { contract.givenArgs.foreach(dispatch) }._1
    val yieldsArgs = variables.collect {contract.yieldsArgs.foreach(dispatch)}._1
    val decreases = contract.decreases.map(element => rewriter.dispatch(element))

    ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs, decreases
      )(contract.blame)(contract.o)
  }

  def rewriteLinearArray(e: Binder[Pre]): Option[Expr[Post]] = {
    val originalBody = e match {
      case Forall(_, _, body) => body
      case Starall(_, _, body) => body
      case _ => return None
    }

    if (e.bindings.exists(_.t != TInt())) return None

    // PB: do not attempt to reshape quantifiers that already have patterns
    if (originalBody.exists { case _: InlinePattern[Pre] => true }) {
      logger.debug(s"Not rewriting $e because it contains patterns")
      return None
    }

    val quantifierData = new RewriteQuantifierData(originalBody, e, this)
    quantifierData.setData()
    quantifierData.checkSingleValueVariables()
    quantifierData.checkIndependentVariables()

    // Check if we have valid bounds to rewrite, otherwise we stop
    if(!quantifierData.checkBounds() || quantifierData.checkOtherBinders()) return quantifierData.result()

    quantifierData.lookForLinearAccesses()
  }

  class RewriteQuantifierData(val bindings: mutable.Set[Variable[Pre]],
                              var lowerBounds: mutable.Map[Variable[Pre], ArrayBuffer[Expr[Pre]]],
                              var upperBounds: mutable.Map[Variable[Pre], ArrayBuffer[Expr[Pre]]],
                              var upperExclusiveBounds: mutable.Map[Variable[Pre], ArrayBuffer[Expr[Pre]]],
                              var independentConditions: ArrayBuffer[Expr[Pre]],
                              val dependentConditions: ArrayBuffer[Expr[Pre]],
                              var body: Expr[Pre],
                              val originalBinder: Binder[Pre],
                              val mainRewriter: SimplifyNestedQuantifiers[Pre]
                             ) {
    def this(originalBody: Expr[Pre], originalBinder: Binder[Pre], rewriter: SimplifyNestedQuantifiers[Pre]) = {
      this(originalBinder.bindings.to(mutable.Set),
        originalBinder.bindings.map(_ -> ArrayBuffer[Expr[Pre]]()).to(mutable.Map),
        originalBinder.bindings.map(_ -> ArrayBuffer[Expr[Pre]]()).to(mutable.Map),
        originalBinder.bindings.map(_ -> ArrayBuffer[Expr[Pre]]()).to(mutable.Map),
        ArrayBuffer[Expr[Pre]](),
        ArrayBuffer[Expr[Pre]](),
        originalBody,
        originalBinder,
        rewriter
      )
    }

    /** Keeps track if it is already feasible to make a new quantifier */
    var newBinder = false

    def setData(): Unit = {
      val allConditions = unfoldBody(Seq())
      // Split bounds that are independent of any binding variables
      val (newIndependentConditions, potentialBounds) = allConditions.partition(indepOf(bindings, _))
      independentConditions.addAll(newIndependentConditions)
      getBounds(potentialBounds)
    }

    def unfoldBody(prevConditions: Seq[Expr[Pre]]): Seq[Expr[Pre]] = {
      val (allConditions, mainBody) = unfoldImplies[Pre](body)
      val newConditions = prevConditions ++ allConditions
      val (newVars, secondBody) = mainBody match {
        case Forall(newVars, _, secondBody) => (newVars, secondBody)
        case Starall(newVars, _, secondBody) => (newVars, secondBody)
        case _ =>
          body = mainBody
          return newConditions
      }

      bindings.addAll(newVars)

      for(v <- newVars){
        lowerBounds(v) = ArrayBuffer[Expr[Pre]]()
        upperBounds(v) = ArrayBuffer[Expr[Pre]]()
        upperExclusiveBounds(v) = ArrayBuffer[Expr[Pre]]()
      }

      body = secondBody

      unfoldBody(newConditions)
    }

    def containsOtherBinders(e: Expr[Pre]): Boolean = {
      e match {
        case _: Binder[Pre] => return true
        case _ => e.transSubnodes.collectFirst { case e: Binder[Pre] => return true }
      }
      false
    }

    /**
      * Process the potential bounds to be either a bound or just a dependent condition.
      * @param potentialBounds Bounds to be processed.
      */
    def getBounds(potentialBounds: Iterable[Expr[Pre]]): Unit = {
      for (bound <- potentialBounds) {
        getSingleBound(bound)
      }
    }

    def getSingleBound(bound: Expr[Pre]): Unit = Comparison.of(bound) match {
      // First try to match a simple comparison
      case Some((_, Comparison.NEQ, _)) => dependentConditions.addOne(bound)
      case Some((left, comp, right)) =>
        if (indepOf(bindings, right)) {
          // x >|>=|==|<=|< 5
          left match {
            case Local(Ref(v)) if bindings.contains(v) => addSingleBound(v, right, comp)
            case Plus(ll, rr) if indepOf(bindings, rr) => getSingleBound(comp.make(ll, right - rr))
            case Plus(ll, rr) if indepOf(bindings, ll) => getSingleBound(comp.make(rr, right - ll))
            case Minus(ll, rr) if indepOf(bindings, rr) => getSingleBound(comp.make(ll, right + rr))
            case _ => dependentConditions.addOne(bound)
          }
        } else if (indepOf(bindings, left)) {
          getSingleBound(comp.flip.make(right, left))
        } else {
          dependentConditions.addOne(bound)
        }
      case None => bound match {
        // If we do not have a simple comparison, we support one special case: i \in {a..b}
        case SeqMember(Local(Ref(v)), Range(from, to))
          if bindings.contains(v) && indepOf(bindings, from) && indepOf(bindings, to) =>
          addSingleBound(v, from, Comparison.GREATER_EQ)
          addSingleBound(v, to, Comparison.LESS)
        case SeqMember(left, Range(from, to)) =>
          getSingleBound(Comparison.GREATER_EQ.make(left, from))
          getSingleBound(Comparison.LESS.make(left, to))
        case _ => dependentConditions.addOne(bound)
      }
    }

    /**
      * Add a bound like v >= right.
      */
    @nowarn("msg=xhaust")
    def addSingleBound(v: Variable[Pre], right: Expr[Pre], comp: Comparison): Unit = {
      right match {
        // Simplify rules from simplify.pvl come up with these kind of rules (specialize_range_right_i),
        // but we want the original bounds
        case Select(Less(e1, e2), e3, e4) =>
          if(e1 == e3 && e2 == e4 || e1 == e4 && e2 == e3){
            addSingleBound(v, e1, comp)
            addSingleBound(v, e2, comp)
            return
          }
        case _ =>
      }

      comp match {
        // v < right
        case Comparison.LESS =>
          upperExclusiveBounds(v).addOne(right)
          upperBounds(v).addOne(right - one)
        // v <= right
        case Comparison.LESS_EQ =>
          upperExclusiveBounds(v).addOne(right + one)
          upperBounds(v).addOne(right)
        // v == right
        case Comparison.EQ =>
          lowerBounds(v).addOne(right)
          upperExclusiveBounds(v).addOne(right + one)
          upperBounds(v).addOne(right)
        // v >= right
        case Comparison.GREATER_EQ => lowerBounds(v).addOne(right)
        // v > right
        case Comparison.GREATER => lowerBounds(v).addOne(right + one)
      }
    }

    /** We check if there now any binding variables which resolve to just a single value, which happens if it
      * has equal lower and upper bounds.
      * E.g. forall(int i,j; i == 0 && i <= j && j < 5; xs[j+i]) ==> forall(int j; 0 <= j < 5; xs[j])
      * We just replace each reference to that value, and check our bounds again.
      * We don't worry if a we have something like x == 5 && x < 0, since that will resolve to 5 < 0, which equally
      * does not work.
      * */
    def checkSingleValueVariables(): Unit = {
      for (name <- bindings) {
        val equalBounds = lowerBounds(name).intersect(upperBounds(name))
        if (equalBounds.nonEmpty) {
          // We will put out a new quantifier
          newBinder = true
          val newValue = equalBounds.head
          val nameVar: Expr[Pre] = Local(name.ref)
          val sub = Substitute[Pre](Map(nameVar -> newValue))
          val replacer = sub.dispatch(_: Expr[Pre])
          body = replacer(body)

          // Do not quantify over name anymore
          bindings.remove(name)

          // Some dependent selects, might now have become independent or even bounds
          val oldDependentBounds = dependentConditions.map(replacer)
          dependentConditions.clear()

          val (new_independentConditions, potentialBounds) = oldDependentBounds.partition(indepOf(bindings, _))
          independentConditions.addAll(new_independentConditions)
          getBounds(potentialBounds)

          // Bounds for the name, have now become independent conditions
          lowerBounds(name).foreach(lb =>
            if (lb != newValue) independentConditions.addOne(LessEq(lb, newValue)))
          upperBounds(name).foreach(ub =>
            if (ub != newValue) independentConditions.addOne(LessEq(newValue, ub)))

          lowerBounds.remove(name)
          upperBounds.remove(name)
          upperExclusiveBounds.remove(name)

          // Strictly speaking, a binding variable could be newly removed, if a previous one has been found constant
          // and then the bounds deem another binding variable also constant. We check that by doing recursion.
          checkSingleValueVariables()
          return
        }
      }
    }

    def checkIndependentVariables(): Unit = {
      for (name <- bindings) {
        if (indepOf(mutable.Set(name), body)) {
          var independent = true
          dependentConditions.foreach(s => if (!indepOf(mutable.Set(name), s)) independent = false)
          if (independent) {
            // We can freely remove this named variable
            val maxBound = extremeValue(name, maximizing = true)
            val minBound = extremeValue(name, maximizing = false)
            (maxBound, minBound) match {
              case (Some(maxBound), Some(minBound)) =>
                newBinder = true
                // Do not quantify over name anymore
                bindings.remove(name)
                lowerBounds.remove(name)
                upperBounds.remove(name)
                upperExclusiveBounds.remove(name)

                // We remove the forall variable i, but need to rewrite some expressions
                // (forall i; a <= i <= b; ...Perm(ar, x)...) ====> b>=a ==> ...Perm(ar, x*(b-a+1))...
                independentConditions.addOne(GreaterEq(maxBound, minBound))

                if(body.t == TResource()){
                  body = Scale(Plus(one, Minus(maxBound, minBound)), body)(
                    PanicBlame("Error in SimplifyNestedQuantifiers class, implication should make sure scale is" +
                      " never negative when accessed."))
                }
              case _ =>
            }
          }
        }
      }
    }

    def extremeValue(name: Variable[Pre], maximizing: Boolean): Option[Expr[Pre]] = {
      if (maximizing && upperBounds(name).nonEmpty)
        Some(extremes(upperBounds(name).toSeq, maximizing))
      else if (!maximizing && lowerBounds(name).nonEmpty)
        Some(extremes(lowerBounds(name).toSeq, maximizing))
      else
        None
    }

    def extremes(xs: Seq[Expr[Pre]], maximizing: Boolean): Expr[Pre] = {
      xs match {
        case expr :: Nil => expr
        case left :: right :: tail =>
          Select(
            condition = if(maximizing) left > right else left < right,
            whenTrue = extremes(left :: tail, maximizing),
            whenFalse = extremes(right :: tail, maximizing),
          )
      }
    }

    // This allows only forall's to be rewritten, if they have at least one lower and upper bound
    def checkBounds(): Boolean = {
      for (name <- bindings) {
        //Exit when notAt least one upper && lower bound
        if (lowerBounds.getOrElse(name, ArrayBuffer()).isEmpty || upperBounds.getOrElse(name, ArrayBuffer()).isEmpty) {
          return false
        }
      }
      true
    }

    // Returns true if contains other binders, which we won't rewrite
    def checkOtherBinders(): Boolean = {
      independentConditions.foldLeft(containsOtherBinders(body))(_ || containsOtherBinders(_))
    }

    case class ForallSubstitute(subs: Map[Variable[Pre], Expr[Post]], indexReplacement: (Expr[Pre], Expr[Post])) extends Rewriter[Pre] {
      override val allScopes = mainRewriter.allScopes

      override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
        case expr if expr == indexReplacement._1  => indexReplacement._2
        case v: Local[Pre] if subs.contains(v.ref.decl) => subs(v.ref.decl)
        case other => rewriteDefault(other)
      }
    }

    def lookForLinearAccesses(): Option[Expr[Post]] = {
      val linearAccesses = new FindLinearArrayAccesses(this)

      mainRewriter.variables.collect {linearAccesses.search(body)} match {
        case (bindings, Some(substituteForall)) =>
          if(bindings.size != 1) throw Unreachable("Only one new variable should be declared with SimplifyNestedQuantifiers.")
          val sub = ForallSubstitute(substituteForall.substituteOldVars, substituteForall.substituteIndex)
          val newBody = sub.dispatch(body)
          val select = Seq(substituteForall.newBounds) ++ independentConditions.map(sub.dispatch) ++
            dependentConditions.map(sub.dispatch)
          val main = if (select.nonEmpty) Implies(AstBuildHelpers.foldAnd(select), newBody) else newBody
          @nowarn("msg=xhaust")
          val forall: Binder[Post] = originalBinder match {
            case _: Forall[Pre] => Forall(bindings, substituteForall.newTriggers, main)(originalBinder.o)
            case originalBinder: Starall[Pre] =>
              Starall(bindings, substituteForall.newTriggers, main)(originalBinder.blame)(originalBinder.o)
          }
          Some(forall)
        case (_, None) => result()
      }
    }

    def result(): Option[Expr[Post]] = {
      // If we changed something we always return a result, even if we could not rewrite further
      val res = if(newBinder) {
        val select = independentConditions ++ dependentConditions
        if (bindings.isEmpty) {
          if (select.isEmpty) Some(body) else Some(Implies(AstBuildHelpers.foldAnd(select.toSeq), body))
        } else {
          upperExclusiveBounds.foreach {
            case (n: Variable[Pre], upperBounds: ArrayBuffer[Expr[Pre]]) =>
              val i: Expr[Pre] = Local(n.ref)
              upperBounds.foreach(upperBound =>
                select.addOne(i < upperBound)
              )
          }
          lowerBounds.foreach {
            case (n: Variable[Pre], lowerBounds: ArrayBuffer[Expr[Pre]]) =>
              val i: Expr[Pre] = Local(n.ref)
              lowerBounds.foreach(lowerBound =>
                select.addOne(lowerBound <= i )
              )
          }
          val newBody = if (select.nonEmpty) Implies(AstBuildHelpers.foldAnd(select.toSeq), body)
          else body

          // TODO: Should we get the old triggers? And then filter if the triggers contain variables which
          //  are not there anymore?
          @nowarn("msg=xhaust")
          val forall: Expr[Pre] = originalBinder match{
            case _: Forall[Pre] => Forall(bindings.toSeq, Seq(), newBody)(originalBinder.o)
            case e: Starall[Pre] => Starall(bindings.toSeq, Seq(), newBody)(e.blame)(originalBinder.o)
          }
          Some(forall)
        }
      } else {
        None
      }

      res.map(mainRewriter.rewriteDefault)
    }
  }

  def indepOf[G](bindings: mutable.Set[Variable[G]], e: Expr[G]): Boolean =
    e.transSubnodes.collectFirst { case Local(ref) if bindings.contains(ref.decl) => () }.isEmpty

  sealed trait Subscript[G] {
    val index: Expr[G]
    val subnodes: Seq[Node[G]]
  }
  case class Array[G](index: Expr[G], subnodes: Seq[Node[G]], array: Expr[G]) extends Subscript[G]

  case class Pointer[G](index: Expr[G], subnodes: Seq[Node[G]], array: Expr[G]) extends Subscript[G]

    class FindLinearArrayAccesses(quantifierData: RewriteQuantifierData){

      // Search for linear array expressions
      def search(e: Node[Pre]): Option[SubstituteForall] = {
        e match {
          case e @ ArrayLocation(_, _) =>
            testSubscript(Array(e.subscript, e.subnodes, e.array))
          case e @ ArraySubscript(_, _)  =>
            testSubscript(Array(e.index, e.subnodes, e.arr))
          case e @ PointerSubscript(_, _)  =>
            testSubscript(Pointer(e.index, e.subnodes, e.pointer))
          case e @ PointerAdd(_, _) =>
            testSubscript(Pointer(e.offset, e.subnodes, e.pointer))
          case _ => e.subnodes.to(LazyList).map(search).collectFirst{case Some(sub) => sub}
        }
      }

      def testSubscript(e: Subscript[Pre]): Option[SubstituteForall] = {
        if (indepOf(quantifierData.bindings, e.index)) {
          return None
        }
        linearExpression(e) match {
          case Some(substituteForall) => Some(substituteForall)
          case None => e.subnodes.to(LazyList).map(search).collectFirst{case Some(sub) => sub}
        }
      }

      def linearExpression(e: Subscript[Pre]): Option[SubstituteForall] = {
        val pot = new PotentialLinearExpressions(e)
        pot.visit(e.index)
        pot.canRewrite()
      }

      class PotentialLinearExpressions(val arrayIndex: Subscript[Pre]){
        val linearExpressions: mutable.Map[Variable[Pre], Expr[Pre]] = mutable.Map()
        var constantExpression: Option[Expr[Pre]] = None
        var isLinear: Boolean  = true
        var currentMultiplier: Option[Expr[Pre]] = None

        def visit(e: Expr[Pre]): Unit = {
          e match{
            case Plus(left, right) =>
              // if the first is constant, the second argument cannot be
              if (isConstant(left)) {
                addToConstant(left)
                visit(right)
              } else if (isConstant(right)) {
                addToConstant(right)
                visit(left)
              } else { // Both arguments contain linear information
                visit(left)
                visit(right)
              }
            case Minus(left, right) =>
              // if the first is constant, the second argument cannot be
              if (isConstant(left)) {
                addToConstant(left)
                val oldMultiplier = currentMultiplier
                multiplyMultiplier(IntegerValue(-1))
                visit(right)
                currentMultiplier = oldMultiplier
              } else if (isConstant(right)) {
                addToConstant(right, isPlus=false)
                visit(left)
              } else { // Both arguments contain linear information
                visit(left)
                val oldMultiplier = currentMultiplier
                multiplyMultiplier(IntegerValue(-1))
                visit(right)
                currentMultiplier = oldMultiplier
              }
            case Mult(left, right) =>
              if (isConstant(left)) {
                val oldMultiplier = currentMultiplier
                multiplyMultiplier(left)
                visit(right)
                currentMultiplier = oldMultiplier
              } else if (isConstant(right)) {
                val oldMultiplier = currentMultiplier
                multiplyMultiplier(right)
                visit(left)
                currentMultiplier = oldMultiplier
              } else {
                isLinear = false
              }
            // TODO: Check if division is right conceptually with an example. Take special care to think about
            //  the order of division
//            case e@FloorDiv(left, right) =>
//              if (isConstant(right)){
//                val oldMultiplier = currentMultiplier
//                multiplyMultiplier(FloorDiv(IntegerValue(1), right)(e.blame))
//                visit(left)
//                currentMultiplier = oldMultiplier
//              } else {
//                isLinear = false
//              }
            case Local(ref) =>
              if(quantifierData.bindings.contains(ref.decl)) {
                linearExpressions get ref.decl match {
                  case None => linearExpressions(ref.decl) = currentMultiplier.getOrElse(IntegerValue(1))
                  case Some(old) => linearExpressions(ref.decl) =
                    Plus(old, currentMultiplier.getOrElse(IntegerValue(1)))
                }
              } else {
                throw Unreachable("We should not end up here, the precondition of \'FindLinearArrayAccesses\' was not uphold.")
              }
            case _ =>
              isLinear = false
          }
        }

        def canRewrite(): Option[SubstituteForall] = {
          if(!isLinear) {
            return None
          }

          // Checking the preconditions of the check_vars_list function
          if(quantifierData.bindings.isEmpty) return None
          for(v <- quantifierData.bindings){
            if(!(linearExpressions.contains(v) &&
              quantifierData.upperExclusiveBounds.contains(v) &&
              quantifierData.upperExclusiveBounds(v).nonEmpty)
            ) {
              return None
            }
          }

          def sortVar(v: Variable[Pre]): Option[BigInt] = equalityChecker.isConstantInt(linearExpressions(v))
          val vars = quantifierData.bindings.toList.sortBy(sortVar)

          val res = vars.permutations.map(check_vars_list)
            .collectFirst({case Some(subst) => subst})
          res
        }

        /**
         * This function determines if the vars in this specific order allow the forall to be rewritten to one
         * forall.
         *
         * Precondition:
         *  * At least one var in `quantifierData.bindings`
         *  * linearExpressions has an expression for all `vars`
         *  * quantifierData.upperExclusiveBounds has a non-empty list for all `vars`
         *  * quantifierData.lowerBounds has a non-empty list for all `vars`
         *
         * We are looking for patterns:
         *   /\_{0 <= i <= k} {xmin_i <= x_i < xmin_i + n_i} : ... ar[Sum_{0 <= i <= k} (a_i * x_i) + b] ...
         * and we require that for i>0
         *   a_i >= a_{i-1} * n_{i-1}
         *   (or equivalent a_i == Prod_{0 <= j < i} {n_j} * a_0 )
         *
         * Further more we require that n_i > 0 and a_i > 0 (although I think a_0<0 is also valid)
         * We can than replace the forall with
         *   off := b + Sum_{0 <= i <= k} (xmin_i * a_i)
         *   0 <= x_new - off < a_k * n_k && (x_new - off) % a_0 == 0 : ... ar[x_new] ...
         * and each x_i gets replaced by
         * base_k -> x_new - off
         * base_{i-1} -> base_i % a_i
         * x_i -> base_i / a_i + xmin_i
         *
         * And for each a_i where a_i > a_{i-1} * n_{i-1} (thus was not equal)
         * We additionally add
         *  base_{i-1} / a_{i-1} < n_{i-1} (derived from (x_{i-1} < xmin_i + n_{i-1})
        */
//          TODO ABOVE
        def check_vars_list(vars: List[Variable[Pre]]): Option[SubstituteForall] = {
          val x0 = vars.head
          val a0 = linearExpressions(x0)
          if(!equalityChecker.isNonZero(a0)) return None
          // x_{i-1}
          var xLast = x0
          var linLast: Expr[Pre] = IntegerValue(0)

          val xmins: mutable.Map[Variable[Pre], Expr[Pre]] = mutable.Map()
          val remainingLowerBounds: mutable.Map[Variable[Pre], Set[Expr[Pre]]] = mutable.Map()
          val remainingUpperBounds: mutable.Map[Variable[Pre], Set[Expr[Pre]]] = mutable.Map()

          for(x <- vars.tail){
            findSuitableBound(x, xLast, linLast) match {
              case None => return None
              case Some(FoundBound(lowerBounds, upperBounds, xmin, linExpr)) =>
                xmins(xLast) = xmin
                remainingLowerBounds(xLast) = lowerBounds
                remainingUpperBounds(xLast) = upperBounds
                xLast = x
                linLast = linExpr
            }
          }
          // We found a replacement!
          // Make the variable & declaration
          val newName = vars.map(_.o.preferredName).mkString("_")
          val xNew = new Variable[Post](TInt())(BinderOrigin(newName))
          quantifierData.mainRewriter.variables.declare(xNew)

          val newGen: Expr[Pre] => Expr[Post] = quantifierData.mainRewriter.dispatch



          // Get a random lowerbound for x_i_last;
          val lowLast = quantifierData.lowerBounds(xLast).head
          xmins(xLast) = lowLast
          remainingLowerBounds(xLast) = quantifierData.lowerBounds(xLast).tail.toSet

          // Get a random upperbound for x_i_last;
          val upLast = quantifierData.upperExclusiveBounds(xLast).head
          remainingUpperBounds(xLast) = quantifierData.upperExclusiveBounds(xLast).tail.toSet
          val nLast = simplifiedMinus(upLast, lowLast)

          // off := b + Sum_{0 <= i <= k} (xmin_i * a_i)
          var offset: Expr[Pre] = constantExpression match {
            case None => simplifiedMult(a0, xmins(x0))
            case Some(b) => simplifiedPlus(b, simplifiedMult(a0, xmins(x0)))
          }

          for(x_i <- vars.tail){
            offset = simplifiedPlus(offset, simplifiedMult(linearExpressions(x_i), xmins(x_i)))
          }

          // base_k == (x_new - off)
          val xNewVar: Expr[Post] = Local(xNew.ref)
          var base: Expr[Post]= if(is_value(offset, 0)) xNewVar else Minus(xNewVar, newGen(offset))
          val replaceMap:  mutable.Map[Variable[Pre], Expr[Post]] = mutable.Map()

          // 0 <= x_new - offset < a_k * n_k
          var newBounds = And(
            LessEq( IntegerValue(0), base),
            Less(base, newGen(simplifiedMult(linearExpressions(xLast), nLast)))
          )

          // Replace the linear expression with the new variable
          val replaceIndex = (arrayIndex.index, xNewVar)

          // and each x_i gets replaced by
          //  x_i -> base_i / a_i + xmin_i
          for(x <- vars.reverse){
            var newValue = base
            val a = linearExpressions(x)
            val xmin = xmins(x)
            if(!is_value(a, 1)) newValue = FloorDiv(newValue, newGen(a))(PanicBlame("a not zero"))
            if(!is_value(xmin, 0)) newValue = Plus(newValue, newGen(xmin))
            replaceMap(x) = newValue

            // base_{i-1} -> base_i % a_i
            if(!is_value(a, 1)) base = Mod(base, newGen(a))(PanicBlame("n not zero"))
          }
          // Add bound that we stride through our forall if a0 != 1
          // (base_1 % a_0 == 0) --> base_0 == 0
          if(!is_value(a0, 1)) newBounds = And(newBounds, Eq(base, IntegerValue(0)))

          val triggerBlame = PanicBlame("Only used as trigger, not as access")
          val newTriggers : Seq[Seq[Expr[Post]]] = arrayIndex match {
            case arrayIndex: Array[Pre] =>
              Seq(Seq(ArraySubscript(newGen(arrayIndex.array), xNewVar)(triggerBlame)),
              )
            case arrayIndex: Pointer[Pre] =>
              Seq(Seq(PointerSubscript(newGen(arrayIndex.array), xNewVar)(triggerBlame)),
                Seq(PointerAdd(newGen(arrayIndex.array), xNewVar)(triggerBlame)))
          }

          for(x <- vars){
            val xNew = replaceMap(x)
            for(lowerBound <- remainingLowerBounds(x)){
              newBounds = And(LessEq(newGen(lowerBound), xNew), newBounds)
            }
            for(upperBound <- remainingUpperBounds(x)){
              newBounds = And(Less(xNew, newGen(upperBound)), newBounds)
            }
          }

          Some(SubstituteForall(newBounds, replaceMap.toMap, replaceIndex, newTriggers))
        }

        case class FoundBound(otherLowerBounds: Set[Expr[Pre]], otherUpperBounds: Set[Expr[Pre]], xMin: Expr[Pre], linExpr: Expr[Pre])

        // Check in the other bounds if the specific expressions is present by a bound
        def isExprUpperBounded(e: Expr[Pre], boundRequired: Expr[Pre]): Boolean = {
          // Determine if l == e
          // Then we know that e <= r (or e < r)
          // Thus if r+1 <= boundRequired ( or r <= boundRequired )
          // We know that e < boundRequired
          def lessEqBound(l: Expr[Pre], r: Expr[Pre], eq: Boolean): Boolean = {
            val checkedR = if(eq) simplifiedPlus(r, IntegerValue(1)) else r
            equalityChecker.equalExpressions(l, e) &&
              equalityChecker.lessThenEq(checkedR, boundRequired).getOrElse(false)
          }

          for(c <- quantifierData.dependentConditions) {
            c match {
              case LessEq(l, r) =>
                if(lessEqBound(l, r, eq = true))
                  return true
              case Less(l, r) =>
                if(lessEqBound(l, r, eq = false)) return true
              case GreaterEq(l, r) =>
                // We switch arguments around
                if(lessEqBound(r, l, eq = true)) return true
              case Greater(l, r) =>
                if(lessEqBound(r, l, eq = false)) return true
              case _ =>
            }
          }
          false
        }

        def findSuitableBound(x: Variable[Pre], xLast: Variable[Pre], linExpr: Expr[Pre]): Option[FoundBound] = {
          val a = linearExpressions(x)
          val aLast = linearExpressions(xLast)

          var otherUpperBounds: Set[Expr[Pre]] = quantifierData.upperExclusiveBounds(xLast).toSet
          var otherLowerBounds: Set[Expr[Pre]] = quantifierData.lowerBounds(xLast).toSet

          for (up <- quantifierData.upperExclusiveBounds(xLast)) {
            for (low <- quantifierData.lowerBounds(xLast)) {
              val nLastCandidate = simplifiedMinus(up, low)

              if (equalityChecker.equalExpressions(a, simplifiedMult(aLast, nLastCandidate))) {
                otherUpperBounds = otherUpperBounds - up
                otherLowerBounds = otherLowerBounds - low
                val linLast = simplifiedPlus(simplifiedMult(aLast, simplifiedMinus(Local(xLast.ref), low)), linExpr)
                return Some(FoundBound(otherLowerBounds, otherUpperBounds, low, linLast))
              }

              if (equalityChecker.lessThenEq(simplifiedMult(aLast, nLastCandidate), a).getOrElse(false)) {
                // This is also valid, we take a stride of a_i, but in that case it will stop earlier
                // So we do not remove the upperbound we found
                otherLowerBounds = otherLowerBounds - low
                val linLast = simplifiedPlus(simplifiedMult(aLast, simplifiedMinus(Local(xLast.ref), low)), linExpr)
                return Some(FoundBound(otherLowerBounds, otherUpperBounds, low, linLast))
              }
            }
          }
          // If we have something like f[8*z + 3*y + x] and the bound 3*y+x<8, we are valid as well
          for (low <- quantifierData.lowerBounds(xLast)) {
            val linLast = simplifiedPlus(simplifiedMult(aLast, simplifiedMinus(Local(xLast.ref), low)), linExpr)
            if(isExprUpperBounded(linLast, a)){
              otherLowerBounds = otherLowerBounds - low
              return Some(FoundBound(otherLowerBounds, otherUpperBounds, low, linLast))
            }
          }

          None
        }


        def getPlusses(e: Expr[Pre]): (Seq[Expr[Pre]], BigInt) = {
          e match {
            case Plus(e1, e2) =>
              val (s1, i1) = getPlusses(e1)
              val (s2, i2) = getPlusses(e2)
              (s1 ++ s2, i1+i2)
            case e => equalityChecker.isConstantInt(e) match {
              case Some(i) => (Seq(), i)
              case None => (Seq(e), 0)
            }
          }
        }

        def simplify(e: Expr[Pre]): Expr[Pre] = {
          val (plusses, value) = getPlusses(e)
          if(value != 0) {
            plusses.foldLeft(IntegerValue(value): Expr[Pre])(Plus[Pre])
          } else if(plusses.isEmpty ) {
            IntegerValue(0)
          } else {
            plusses.reduce(Plus[Pre])
          }
        }

        def simplifiedMinus(lhs_arg: Expr[Pre], rhs_arg: Expr[Pre]) : Expr[Pre] = {
          val lhs = simplify(lhs_arg)
          val rhs = simplify(rhs_arg)
          (equalityChecker.isConstantInt(lhs), equalityChecker.isConstantInt(rhs)) match {
            case (Some(l), Some(r)) => return IntegerValue(l - r)
            case (_, Some(r)) if r == 0 => return lhs
            case _ =>
          }
          (lhs, rhs) match {
            case (Plus(l, r), rhs) =>
              if (equalityChecker.equalExpressions(l, rhs)) return r
              else if (equalityChecker.equalExpressions(r, rhs)) return l
            case _ =>
          }
          (lhs, rhs) match {
            case (Plus(l1, r1), Plus(l2, r2)) =>
              if(equalityChecker.equalExpressions(l1 ,l2)) return Minus(r1, r2)
              else if(equalityChecker.equalExpressions(l1 ,r2)) return Minus(r1, l2)
              else if(equalityChecker.equalExpressions(r1 ,l2)) return Minus(l1, r2)
              else if(equalityChecker.equalExpressions(r1 ,r2)) return Minus(l1, l2)
            case _ =>
          }

          (lhs, rhs) match {
            case (Mult(l1, r1), Mult(l2, r2)) =>
              if(equalityChecker.equalExpressions(l1 ,l2)) return Mult(l1, simplifiedMinus(r1, r2))
              else if(equalityChecker.equalExpressions(l1 ,r2)) return Mult(l1, simplifiedMinus(r1, l2))
              else if(equalityChecker.equalExpressions(r1 ,l2)) return Mult(r1, simplifiedMinus(l1, r2))
              else if(equalityChecker.equalExpressions(r1 ,r2)) return Mult(r1, simplifiedMinus(l1, l2))
            case _ =>
          }

          Minus(lhs, rhs)
        }

        def simplifiedPlus(lhs: Expr[Pre], rhs: Expr[Pre]) : Expr[Pre] = {
          (equalityChecker.isConstantInt(lhs), equalityChecker.isConstantInt(rhs)) match {
            case (Some(l), Some(r)) => IntegerValue(l + r)
            case (_, Some(r)) if r == 0 => lhs
            case (Some(l), _) if l == 0 => rhs
            case _ => Plus(lhs, rhs)
          }
        }

        def simplifiedMult(lhs: Expr[Pre], rhs: Expr[Pre]): Expr[Pre] = {
          if (is_value(lhs, 1)) rhs
          else if (is_value(rhs, 1)) lhs
          else Mult(lhs, rhs)
        }

        def isConstant(node: Expr[Pre]): Boolean = indepOf(quantifierData.bindings, node)

        def addToConstant(node : Expr[Pre], isPlus: Boolean = true): Unit = {
          val added_node: Expr[Pre] = currentMultiplier match  {
            case None => node
            case Some(expr) => Mult(expr, node)
          }
          constantExpression = Some(constantExpression match {
            case None => if(isPlus) added_node else Mult(IntegerValue(-1), added_node)
            case Some(expr) => if(isPlus) Plus(expr, added_node) else Minus(expr, added_node)
          })
        }

        def multiplyMultiplier(node : Expr[Pre]): Unit ={
          currentMultiplier match {
            case None => currentMultiplier = Some(node);
            case Some(expr) => currentMultiplier = Some(Mult(expr, node))
          }
        }

        def is_value(e: Expr[Pre], x: Int): Boolean =
          equalityChecker.isConstantInt(e) match {
            case None => false
            case Some(y) => y == x
          }
      }
    }

  // The `newBounds`, will contain all the new equations for "select" part of the forall.
  // The `substituteOldVars` contains a map, so we can replace the old forall variables with new expressions
  // We also store the `linearExpression`, so if we ever come across it, we can replace it with the new variable.
  case class SubstituteForall(newBounds: Expr[Post], substituteOldVars: Map[Variable[Pre], Expr[Post]], substituteIndex: (Expr[Pre], Expr[Post]), newTriggers: Seq[Seq[Expr[Post]]])
}