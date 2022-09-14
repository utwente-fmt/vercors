package vct.col.newrewrite

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{ArraySubscript, _}
import vct.col.ast.util.{AnnotationVariableInfoGetter, ExpressionEqualityCheck}
import vct.col.newrewrite.util.Comparison
import vct.col.origin.{Origin, PanicBlame}
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
              case Starall(_, triggers, _) if triggers.isEmpty => logger.warn(f"The binder '$res' contains no triggers")
              case Forall(_, triggers, _) if triggers.isEmpty => logger.warn(f"The binder '$res' contains no triggers")
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
        // First try to match a simple comparison
        Comparison.of(bound) match {
          case Some((_, Comparison.NEQ, _)) => dependentConditions.addOne(bound)
          case Some((left, comp, right)) =>
            if (indepOf(bindings, right)) {
              // x >|>=|==|<=|< 5
              left match {
                case Local(Ref(v)) if bindings.contains(v) => addSingleBound(v, right, comp)
                case _ => dependentConditions.addOne(bound)
              }
            } else if (indepOf(bindings, left)) {
              right match {
                case Local(Ref(v)) if bindings.contains(v) =>
                  // If the quantified variable is the second argument: flip the relation
                  addSingleBound(v, left, comp.flip)
                case _ => dependentConditions.addOne(bound)
              }
            }
          case None => bound match {
            // If we do not have a simple comparison, we support one special case: i \in {a..b}
            case SeqMember(Local(Ref(v)), Range(from, to))
              if bindings.contains(v) && indepOf(bindings, from) && indepOf(bindings, to) =>
              addSingleBound(v, from, Comparison.GREATER_EQ)
              addSingleBound(v, to, Comparison.LESS)
            case _ => dependentConditions.addOne(bound)
          }
        }
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
          upperBounds(v).addOne(right + one)
        // v <= right
        case Comparison.LESS_EQ =>
          upperExclusiveBounds(v).addOne(right - one)
          upperBounds(v).addOne(right)
        // v == right
        case Comparison.EQ =>
          lowerBounds(v).addOne(right)
          upperExclusiveBounds(v).addOne(right - one)
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

    // This allows only forall's to be rewritten, if they have at least one lower bound of zero
    // TODO: Generalize this, so we don't have this restriction
    def checkBounds(): Boolean = {
      if(bindings.size == 1) {
        val name = bindings.head
        return upperBounds.getOrElse(name, ArrayBuffer()).nonEmpty &&
          lowerBounds.getOrElse(name, ArrayBuffer()).nonEmpty
      }

      val zero = BigInt(0)
      for (name <- bindings) {
        def hasZero: Boolean = {
            lowerBounds.getOrElse(name, ArrayBuffer())
            .foreach(lower => equalityChecker.isConstantInt(lower) match {
            case Some(`zero`) => return true
            case _ =>
          })
        false
        }

        //Exit when notAt least one zero, or no upper bounds
        if (!hasZero || upperBounds.getOrElse(name, ArrayBuffer()).isEmpty) {
          return false
        }
      }
      true
    }

    // Returns true if contains other binders, which we won't rewrite
    def checkOtherBinders(): Boolean = {
      independentConditions.foldLeft(containsOtherBinders(body))(_ || containsOtherBinders(_))
    }

    case class ForallSubstitute(subs: Map[Expr[Pre], Expr[Post]])
      extends Rewriter[Pre] {
      override val allScopes = mainRewriter.allScopes

      override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
        case expr if subs.contains(expr) => subs(expr)
        case other => rewriteDefault(other)
      }
    }

    def lookForLinearAccesses(): Option[Expr[Post]] = {
      val linearAccesses = new FindLinearArrayAccesses(this)

      mainRewriter.variables.collect {linearAccesses.search(body)} match {
        case (bindings, Some(substituteForall)) =>
          if(bindings.size != 1){
            Unreachable("Only one new variable should be declared with SimplifyNestedQuantifiers.")
          }
          val sub = ForallSubstitute(substituteForall.substituteOldVars)
          val newBody = sub.dispatch(body)
          val select = Seq(substituteForall.newBounds) ++ independentConditions.map(sub.dispatch) ++
            dependentConditions.map(sub.dispatch)
          val main = if (select.nonEmpty) Implies(AstBuildHelpers.foldAnd(select), newBody) else newBody
          @nowarn("msg=xhaust")
          val forall: Binder[Post] = originalBinder match {
            case _: Forall[Pre] => Forall(bindings, substituteForall.newTrigger, main)(originalBinder.o)
            case originalBinder: Starall[Pre] =>
              Starall(bindings, substituteForall.newTrigger, main)(originalBinder.blame)(originalBinder.o)
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
          val new_body = if (select.nonEmpty) Implies(AstBuildHelpers.foldAnd(select.toSeq), body)
          else body

          // TODO: Should we get the old triggers? And then filter if the triggers contain variables which
          //  are not there anymore?
          @nowarn("msg=xhaust")
          val forall: Expr[Pre] = originalBinder match{
            case _: Forall[Pre] => Forall(bindings.toSeq, Seq(), new_body)(originalBinder.o)
            case e: Starall[Pre] => Starall(bindings.toSeq, Seq(), new_body)(e.blame)(originalBinder.o)
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
  case class Array[G](e: ArraySubscript[G]) extends Subscript[G] {
    val index: Expr[G] = e.index
    val subnodes: Seq[Node[G]] = e.subnodes
  }

  case class Pointer[G](e: PointerSubscript[G]) extends Subscript[G] {
    val index: Expr[G] = e.index
    val subnodes: Seq[Node[G]] = e.subnodes
  }

    class FindLinearArrayAccesses(quantifierData: RewriteQuantifierData){

      // Search for linear array expressions
      def search(e: Node[Pre]): Option[SubstituteForall] = {
        e match {
          case e @ ArraySubscript(_, _)  =>
            testSubscript(Array(e))
          case e @ PointerSubscript(_, _)  =>
            testSubscript(Pointer(e))
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
                val old_multiplier = currentMultiplier
                multiplyMultiplier(IntegerValue(-1))
                visit(right)
                currentMultiplier = old_multiplier
              } else if (isConstant(right)) {
                addToConstant(right, is_plus=false)
                visit(left)
              } else { // Both arguments contain linear information
                visit(left)
                val old_multiplier = currentMultiplier
                multiplyMultiplier(IntegerValue(-1))
                visit(right)
                currentMultiplier = old_multiplier
              }
            case Mult(left, right) =>
              if (isConstant(left)) {
                val old_multiplier = currentMultiplier
                multiplyMultiplier(left)
                visit(right)
                currentMultiplier = old_multiplier
              } else if (isConstant(right)) {
                val old_multiplier = currentMultiplier
                multiplyMultiplier(right)
                visit(left)
                currentMultiplier = old_multiplier
              } else {
                isLinear = false
              }
            // TODO: Check if division is right conceptually with an example. Take special care to think about
            //  the order of division
            case e@Div(left, right) =>
              if (isConstant(right)){
                val old_multiplier = currentMultiplier
                multiplyMultiplier(Div(IntegerValue(1), right)(e.blame))
                visit(left)
                currentMultiplier = old_multiplier
              } else {
                isLinear = false
              }
            case Local(ref) =>
              if(quantifierData.bindings.contains(ref.decl)) {
                linearExpressions get ref.decl match {
                  case None => linearExpressions(ref.decl) = currentMultiplier.getOrElse(IntegerValue(1))
                  case Some(old) => linearExpressions(ref.decl) =
                    Plus(old, currentMultiplier.getOrElse(IntegerValue(1)))
                }
              } else {
                Unreachable("We should not end up here, the precondition of \'FindLinearArrayAccesses\' was not uphold.")
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

          quantifierData.bindings.toList.reverse.permutations.map(check_vars_list)
            .collectFirst({case Some(subst) => subst})
        }

        /**
          * This function determines if the vars in this specific order allow the forall to be rewritten to one
          * forall.
          *
          * Precondition:
          *   * At least one var in `quantifierData.bindings`
          *   * linearExpressions has an expression for all `vars`
          *   * quantifierData.upperExclusiveBounds has a non-empty list for all `vars`
          *
          * We are looking for patterns:
          *   /\_{0 <= i < k} {0 <= x_i < n_i} : ... ar[Sum_{0 <= i < k} {a_i * x_i} + b] ...
          * and we require that for i>0
          *   a_i == a_{i-1} * n_{i-1}
          *   (or equivalent a_i == Prod_{0 <= j < i} {n_j} * a_0 )
          *
          * Further more we require that n_i > 0 and a_i > 0 (although I think a_0<0 is also valid)
          * We can than replace the forall with
          *   b <= x_new < a_{k-1} * n_{k-1} + b && (x_new - b) % a_0 == 0 : ... ar[x_new] ...
          * and each x_i gets replaced by
          *   x_i -> ((x_new - b) / a_i) % n_i
          * And since we never go past a_{k-1} * n_{k-1} + b, no modulo needed here
          *   x_{k-1} -> (x_new - b) / a_{k-1}
          */
        def check_vars_list(vars: List[Variable[Pre]]): Option[SubstituteForall] = {
          val x_0 = vars.head
          val a_0 = linearExpressions(x_0)
          if(!equalityChecker.isNonZero(a_0)) return None
          // x_{i-1}, a_{i-1}, n_{i-1}
          var x_i_last = x_0
          var a_i_last = a_0
          var n_i_last: Expr[Pre] = null
          val ns: mutable.Map[Variable[Pre], Expr[Pre]] = mutable.Map()

          val new_name = vars.map(_.o.preferredName).mkString("_")
          val x_new = new Variable[Post](TInt())(BinderOrigin(new_name))


          val newGen: Expr[Pre] => Expr[Post] = quantifierData.mainRewriter.dispatch

          // x_base == (x_new -b)
          val x_base: Expr[Post]= constantExpression match {
            case None => Local(x_new.ref)
            case Some(b) => Minus(Local(x_new.ref), newGen(b))
          }
          val replace_map:  mutable.Map[Expr[Pre], Expr[Post]] = mutable.Map()

          for(x_i <- vars.tail){
            val a_i = linearExpressions(x_i)
            var found_valid_n = false

            // Find a suitable upper bound
            for (n_i_last_candidate <- quantifierData.upperExclusiveBounds(x_i_last)) {
              if( !found_valid_n && /*equalityChecker.isGreaterThanZero(n_i_last_candidate) && */
                equalityChecker.equalExpressions(a_i, simplified_mult(a_i_last, n_i_last_candidate)) ) {
                found_valid_n = true
                n_i_last = n_i_last_candidate
                ns(x_i_last) = n_i_last_candidate
              }
            }

            if(!found_valid_n) return None
            // We now know the valid bound of x_{i-1}
            //  x_{i-1} -> ((x_new -b) / a_{i-1}) % n_{i-1}

            replace_map(Local(x_i_last.ref)) =
              if(is_value(a_i_last, 1))
                Mod(x_base, newGen(n_i_last))(PanicBlame("Error in SimplifyNestedQuantifiers, n_i_last should not be zero"))
              else
                Mod(FloorDiv(x_base, newGen(a_i_last))(
                  PanicBlame("Error in SimplifyNestedQuantifiers, a_i_last should not be zero")),
                  newGen(n_i_last))(PanicBlame("Error in SimplifyNestedQuantifiers, n_i_last should not be zero"))

            // Yay we are good up to now, go check out the next i
            x_i_last = x_i
            a_i_last = a_i
            n_i_last = null
          }
          // We found a replacement!
          // Make the declaration final
          quantifierData.mainRewriter.variables.declare(x_new)

          // Replace the linear expression with the new variable
          val x_new_var: Expr[Post] = Local(x_new.ref)
          replace_map(arrayIndex.index) = x_new_var
          val trigger: Expr[Post] = arrayIndex match {
            case Array( node @ ArraySubscript(arr, _)) => ArraySubscript(newGen(arr), x_new_var)(node.blame)
            case Pointer( node @ PointerSubscript(arr, _)) => PointerSubscript(newGen(arr), x_new_var)(node.blame)
          }
          val newTrigger : Seq[Seq[Expr[Post]]] = Seq(Seq(trigger))

          // Add the last value, no need to do modulo
          //
          replace_map(Local(x_i_last.ref)) = if(is_value(a_i_last, 1)) x_base
          else FloorDiv(x_base, newGen(a_i_last))(PanicBlame("Error in SimplifyNestedQuantifiers, a_i_last should not be zero"))

          // Get a random upperbound for x_i_last;
          n_i_last = quantifierData.upperExclusiveBounds(x_i_last).head
          ns(x_i_last) = n_i_last
          // 0 <= x_new - b < a_{k-1} * n_{k-1}
          var new_bounds = And(
            LessEq( IntegerValue(0), x_base),
            Less(x_base, newGen(simplified_mult(a_i_last, n_i_last)))
          )
          // && (x_new - b) % a_0 == 0
          new_bounds = if(is_value(a_0, 1)) new_bounds else
            And(new_bounds,
              //TODO
              Eq( Mod(x_base, newGen(a_0))(PanicBlame("Error in SimplifyNestedQuantifiers, a_0 should not be zero")),
              IntegerValue(0))
          )

          for(x_i <- vars){
            val n_i = ns(x_i)
            // Remove the upper bound we used, but keep the others
            for(old_upper_bound <- quantifierData.upperExclusiveBounds(x_i)){
              if(old_upper_bound != n_i){
                new_bounds = And(Less(replace_map(Local(x_i.ref)), newGen(old_upper_bound)), new_bounds)
              }
            }

            // Remove the lower zero bound, but keep the others
            for(old_lower_bound <- quantifierData.lowerBounds(x_i))
              if(!is_value(old_lower_bound, 0))
                new_bounds = And(LessEq(newGen(old_lower_bound), replace_map(Local(x_i.ref))), new_bounds)

            // Since we know the lower bound was also 0, and the we multiply the upper bounds,
            // we do have to require that each upper bound is at least bigger than 0.
            new_bounds = And(Less(IntegerValue(0), newGen(n_i)), new_bounds)
          }

          Some(SubstituteForall(new_bounds, replace_map.toMap, newTrigger))
        }

        def simplified_mult(lhs: Expr[Pre], rhs: Expr[Pre]): Expr[Pre] = {
          if (is_value(lhs, 1)) rhs
          else if (is_value(rhs, 1)) lhs
          else Mult(lhs, rhs)
        }

        def isConstant(node: Expr[Pre]): Boolean = indepOf(quantifierData.bindings, node)

        def addToConstant(node : Expr[Pre], is_plus: Boolean = true): Unit = {
          val added_node: Expr[Pre] = currentMultiplier match  {
            case None => node
            case Some(expr) => Mult(expr, node)
          }
          constantExpression = Some(constantExpression match {
            case None => if(is_plus) added_node else Mult(IntegerValue(-1), added_node)
            case Some(expr) => if(is_plus) Plus(expr, added_node) else Minus(expr, added_node)
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
  case class SubstituteForall(newBounds: Expr[Post], substituteOldVars: Map[Expr[Pre], Expr[Post]], newTrigger: Seq[Seq[Expr[Post]]])
}