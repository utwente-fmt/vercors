package vct.rewrite.rasi

import hre.io.RWFile
import vct.col.ast._
import vct.col.util.{AstBuildHelpers, Substitute}

import java.io.Writer
import java.nio.file.Path

case object Utils {
  def abs_max(a: Int, b: Int): Int = Seq(-a, a, -b, b).max

  def prod_max(a1: Int, a2: Int, b1: Int, b2: Int): Int = Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).max

  def prod_min(a1: Int, a2: Int, b1: Int, b2: Int): Int = Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).min

  def loop_contract_to_expression[G](contract: LoopContract[G]): Expr[G] = contract match {
    case LoopInvariant(inv, _) => inv
  }

  def contract_to_expression[G](contract: AccountedPredicate[G]): Expr[G] =
    AstBuildHelpers.unfoldPredicate(contract).reduce((e1, e2) => Star(e1, e2)(e1.o))

  def unify_expression[G](cond: Expr[G], args: Map[Variable[G], Expr[G]]): Expr[G] =
    Substitute(args.map[Expr[G], Expr[G]]{ case (v, e) => Local[G](v.ref)(v.o) -> Old(e, None)(e.o)(e.o) }).dispatch(cond)

  def contains_global_invariant[G](node: Node[G]): Boolean = node match {
    case InstancePredicateApply(_, ref, _, _) =>
      if (ref.decl.o.getPreferredName.get.snake.equals("global_invariant")) true
      else contains_global_invariant(ref.decl.body.getOrElse(BooleanValue(value = true)(node.o)))
    case e: Expr[G] => e.subnodes.exists(n => contains_global_invariant(n))
    case _ => false
  }

  def print[G](states: Seq[AbstractState[G]], edges: Seq[(AbstractState[G], AbstractState[G])], out: Path): Unit = {
    val node_names: Map[AbstractState[G], String] = Map.from(states.zipWithIndex.map(t => (t._1, s"n${t._2}")))
    RWFile(out.toFile).write(w => print_state_space(node_names, edges, w))
  }

  private def print_state_space[G](names: Map[AbstractState[G], String], edges: Seq[(AbstractState[G], AbstractState[G])], writer: Writer): Unit = {
    writer.append("digraph {\n")
    names.foreach(t => writer.append(t._2)
                             .append(s"[label=${"\""}")
                             .append(t._1.to_expression.toInlineString)
                             .append(s"${"\""}];\n"))
    edges.foreach(t => writer.append(names(t._1))
                             .append(" -> ")
                             .append(names(t._2))
                             .append(";\n"))
    writer.append("}")
  }
}
