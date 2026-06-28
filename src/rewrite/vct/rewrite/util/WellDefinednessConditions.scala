package vct.col.rewrite.util

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers.{foldStar, unfoldStar}
import vct.col.util.Substitute

/** Collects the precondition of each pure function called within `expr`, args
  * substituted for formals, ordered deepest-first so Viper can evaluate each in
  * the context the prior ones established.
  */
object WellDefinednessConditions {

  def collect[G](
      expr: Expr[G],
      visited: Set[AbstractFunction[G]] = Set.empty[AbstractFunction[G]],
  ): Seq[Expr[G]] = {
    // Add the current function to visited before recursing into subnodes, so
    // that recursive/mutually-recursive calls are not expanded again.
    val newVisited: Set[AbstractFunction[G]] =
      expr match {
        case inv: FunctionInvocation[G] => visited + inv.ref.decl
        case inv: InstanceFunctionInvocation[G] => visited + inv.ref.decl
        case _ => visited
      }

    val fromSubnodes: Seq[Expr[G]] = expr.subnodes.collect { case e: Expr[G] =>
      e
    }.flatMap(collect(_, newVisited))

    val direct: Seq[Expr[G]] =
      expr match {
        case inv: FunctionInvocation[G] if !visited.contains(inv.ref.decl) =>
          val decl = inv.ref.decl
          implicit val o: Origin = decl.contract.o
          val pre = foldStar(decl.contract.requires)
          val argSubs: Map[Expr[G], Expr[G]] =
            decl.args.zip(inv.args).map { case (param, actual) =>
              (Local[G](param.ref)(param.o): Expr[G]) -> actual
            }.toMap
          unfoldStar(Substitute[G](argSubs).dispatch(pre))

        case inv: InstanceFunctionInvocation[G]
            if !visited.contains(inv.ref.decl) =>
          val decl = inv.ref.decl
          implicit val o: Origin = decl.contract.o
          val pre = foldStar(decl.contract.requires)
          // Substitute 'this' with the actual receiver. ThisObject equality is
          // based on its cls field only (origin is in the second parameter list),
          // so one map entry covers all ThisObject nodes for this class.
          val thisOpt = pre.collect { case t: ThisObject[G] => t }.headOption
          val thisSubs: Map[Expr[G], Expr[G]] =
            thisOpt.map(t => (t: Expr[G]) -> inv.obj).toMap
          val argSubs: Map[Expr[G], Expr[G]] =
            decl.args.zip(inv.args).map { case (param, actual) =>
              (Local[G](param.ref)(param.o): Expr[G]) -> actual
            }.toMap
          unfoldStar(Substitute[G](thisSubs ++ argSubs).dispatch(pre))

        case _ => Nil
      }

    fromSubnodes ++ direct
  }

  /** Splits a `**`-conjoined contract predicate (requires/ensures) into its
    * individual conjuncts.
    */
  def splitPred[G](pred: AccountedPredicate[G]): Seq[Expr[G]] =
    pred match {
      case UnitAccountedPredicate(e) => unfoldStar(e)
      case SplitAccountedPredicate(l, r) => splitPred(l) ++ splitPred(r)
    }

  /** Whether an expression is free of heap-dependent constructs (permissions,
    * field/array/model dereferences, predicates, etc.), i.e. it can be safely
    * inhaled into a heap state different from the one it was taken from.
    */
  def isNonHeap[G](expr: Expr[G]): Boolean =
    expr match {
      case BooleanValue(false) => false
      case _ =>
        !expr.exists {
          case _: Perm[G] | _: PointsTo[G] | _: Value[G] | _: CurPerm[G] => true
          case _: Scale[G] | _: ScaleByParBlock[G] => true
          case _: PermPointer[G] | _: PermPointerIndex[G] => true
          case _: PredicateApplyExpr[G] | _: ForPerm[G] |
              _: ForPermWithValue[G] =>
            true
          case _: Held[G] | _: Wand[G] | _: Committed[G] => true
          case _: Invocation[G] => true
          case _: HeapDeref[G] | _: DerefPointer[G] | _: ModelDeref[G] => true
          case _: ArraySubscript[G] => true
          case _: Unfolding[G] => true
          case _: ModelPerm[G] | _: ActionPerm[G] => true
          case _: ResourceOfResourceValue[G] | _: ResourceValue[G] => true
        }
    }

  /** Extracts the heap-free part of `expr`, descending into
    * `&&`/`**`/`==>`/`?:` so a pure conjunct isn't lost just because it's
    * paired with a heap-dependent one. `None` if nothing can be extracted.
    */
  def extractNonHeap[G](expr: Expr[G]): Option[Expr[G]] = {
    implicit val o: Origin = expr.o
    expr match {
      case _ if isNonHeap(expr) => Some(expr)
      case And(l, r) =>
        (extractNonHeap(l), extractNonHeap(r)) match {
          case (Some(l2), Some(r2)) => Some(And(l2, r2))
          case (Some(l2), None) => Some(l2)
          case (None, Some(r2)) => Some(r2)
          case (None, None) => None
        }
      case Star(l, r) =>
        (extractNonHeap(l), extractNonHeap(r)) match {
          case (Some(l2), Some(r2)) => Some(And(l2, r2))
          case (Some(l2), None) => Some(l2)
          case (None, Some(r2)) => Some(r2)
          case (None, None) => None
        }
      case Implies(c, r) if isNonHeap(c) => extractNonHeap(r).map(Implies(c, _))
      case Select(c, l, r) if isNonHeap(c) =>
        (extractNonHeap(l), extractNonHeap(r)) match {
          case (Some(l2), Some(r2)) => Some(Select(c, l2, r2))
          case (Some(l2), None) => Some(Implies(c, l2))
          case (None, Some(r2)) => Some(Implies(Not(c), r2))
          case (None, None) => None
        }
      case _ => None
    }
  }
}
