package vct.col.rewrite.util

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers.{foldStar, unfoldStar}
import vct.col.util.Substitute

/** Collect the well-definedness conditions needed to evaluate an expression in a
  * specification. For each pure function call in the expression, the function's
  * precondition (with actual arguments substituted for formals) is included.
  * Deeper conditions come before shallower ones so that Viper can evaluate each
  * condition in the context established by prior ones.
  */
object WellDefinednessConditions {

  def collect[G](expr: Expr[G], visited: Set[scala.Any] = Set.empty): Seq[Expr[G]] = {
    // Add the current function to visited before recursing into subnodes, so
    // that recursive/mutually-recursive calls are not expanded again.
    val newVisited: Set[scala.Any] = expr match {
      case inv: FunctionInvocation[G]         => visited + inv.ref.decl
      case inv: InstanceFunctionInvocation[G] => visited + inv.ref.decl
      case _                                  => visited
    }

    // Recurse into direct sub-expressions first (depth-first) so their
    // conditions appear before the conditions of the current call.
    val fromSubnodes: Seq[Expr[G]] =
      expr.subnodes.collect { case e: Expr[G] => e }
        .flatMap(collect(_, newVisited))

    // Collect conditions contributed by the current node.
    val direct: Seq[Expr[G]] = expr match {
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
}
