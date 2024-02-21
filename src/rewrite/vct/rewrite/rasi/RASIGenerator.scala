package vct.rewrite.rasi

import vct.col.ast.{Expr, IntType, Or, TBool}
import vct.rewrite.cfg.CFGNode

import java.nio.file.Path
import scala.collection.immutable.HashMap
import scala.collection.mutable

case class RASIGenerator[G]() {
  private val found_states: mutable.ArrayBuffer[AbstractState[G]] = mutable.ArrayBuffer()
  private val found_edges: mutable.ArrayBuffer[(AbstractState[G], AbstractState[G])] = mutable.ArrayBuffer()
  private val current_branches: mutable.ArrayBuffer[AbstractState[G]] = mutable.ArrayBuffer()

  def generate_rasi(node: CFGNode[G], vars: Set[ConcreteVariable[G]]): Expr[G] = {
    explore(node, vars)
    found_states.distinctBy(s => s.valuations).map(s => s.to_expression).reduce((e1, e2) => Or(e1, e2)(e1.o))
  }

  def print_state_space(node: CFGNode[G], vars: Set[ConcreteVariable[G]], out_path: Path): Unit = {
    explore(node, vars)
    val (ns, es) = reduce_redundant_states()
    Utils.print(ns, es, out_path)
  }

  private def explore(node: CFGNode[G], vars: Set[ConcreteVariable[G]]): Unit = {
    val initial_state = AbstractState(get_initial_values(vars), HashMap((AbstractProcess[G]("main"), node)), None)
    found_states += initial_state
    current_branches += initial_state

    while (current_branches.nonEmpty) {
      val curr: AbstractState[G] = current_branches.head
      current_branches -= curr

      val successors: Set[AbstractState[G]] = curr.successors()
      found_edges.addAll(successors.map(s => (curr, s)))
      successors.foreach(s => if (!found_states.contains(s)) {found_states += curr; current_branches += curr})
    }
  }

  private def get_initial_values(vars: Set[ConcreteVariable[G]]): Map[ConcreteVariable[G], UncertainValue] = {
    // TODO: Should this be uncertain or should it be defined (e.g. 0/false)?
    Map.from(vars.map(v => (v, v.t match {
      case _: IntType[_] => UncertainIntegerValue.uncertain()
      case _: TBool[_] => UncertainIntegerValue.uncertain()
    })))
  }

  private def reduce_redundant_states(): (Seq[AbstractState[G]], Seq[(AbstractState[G], AbstractState[G])]) = {
    val state_groups: Map[AbstractState[G], AbstractState[G]] = found_states.groupBy(s => s.valuations).flatMap(t => t._2.map(s => (s, t._2.head)))
    val edge_groups: Seq[(AbstractState[G], AbstractState[G])] = Seq.from(found_edges.map(t => (state_groups(t._1), state_groups(t._2))).distinct)
    (state_groups.keySet.toSeq, edge_groups)
  }
}
