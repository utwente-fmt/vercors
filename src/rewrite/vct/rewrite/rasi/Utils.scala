package vct.rewrite.rasi

import hre.io.RWFile

import java.io.Writer
import java.nio.file.Path

case object Utils {
  def abs_max(a: Int, b: Int): Int = Seq(-a, a, -b, b).max

  def prod_max(a1: Int, a2: Int, b1: Int, b2: Int): Int = Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).max

  def prod_min(a1: Int, a2: Int, b1: Int, b2: Int): Int = Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).min

  def print[G](states: Seq[AbstractState[G]], edges: Seq[(AbstractState[G], AbstractState[G])], out: Path): Unit = {
    val node_names: Map[AbstractState[G], String] = Map.from(states.zipWithIndex.map(t => (t._1, s"n${t._2}")))
    RWFile(out.toFile).write(w => print_state_space(node_names, edges, w))
  }

  private def print_state_space[G](names: Map[AbstractState[G], String], edges: Seq[(AbstractState[G], AbstractState[G])], writer: Writer): Unit = {
    names.foreach(t => writer.append(t._2)
                             .append(s"[label=${"\""}")
                             .append(t._1.to_expression.toInlineString)
                             .append(s"${"\""}];"))
    edges.foreach(t => writer.append(names(t._1))
                             .append(" -> ")
                             .append(names(t._2))
                             .append(";"))
  }
}
