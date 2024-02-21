package vct.rewrite.rasi

import vct.col.ast.{Expr, IntType, Or, TBool}
import vct.rewrite.cfg.CFGNode

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

  private def get_initial_values(vars: Set[ConcreteVariable[G]]): Map[ConcreteVariable[G], UncertainValue] =
    Map.from(vars.map(v => (v, v.t match {
      case _: IntType[_] => UncertainIntegerValue.uncertain()
      case _: TBool[_] => UncertainIntegerValue.uncertain()
    })))
}
