package vct.rewrite.rasi

import vct.col.ast.{BooleanValue, Expr, Or}
import vct.col.origin.{LabelContext, Origin}
import vct.rewrite.cfg.CFGNode

import scala.collection.immutable.HashMap
import scala.collection.mutable

case class RASIGenerator[G]() {
  private val found_states: mutable.Set[AbstractState[G]] = mutable.LinkedHashSet()
  private val current_branches: mutable.Set[AbstractState[G]] = mutable.LinkedHashSet()

  private def neutral_element: Expr[G] = BooleanValue(value = false)(Origin(Seq(LabelContext("neutral element of or"))))
  def generate_rasi(node: CFGNode[G], vars: Set[ConcreteVariable[G]]): Expr[G] = {
    explore(node, vars)
    found_states.map(a => a.to_expression()).fold(neutral_element)((e1, e2) => Or(e1, e2)(e1.o))
  }

  private def explore(node: CFGNode[G], vars: Set[ConcreteVariable[G]]): Unit = {
    val initial_state = AbstractState(get_initial_values(vars), HashMap((AbstractProcess[G]("main"), node)))
    found_states.addOne(initial_state)
    current_branches.addOne(initial_state)

    while (current_branches.nonEmpty) {
      val curr = current_branches.head
      current_branches.remove(curr)

      curr.successors().foreach(s => if (!found_states.contains(s)) {found_states.addOne(curr); current_branches.addOne(curr)})
    }
  }

  private def get_initial_values(vars: Set[ConcreteVariable[G]]): Map[ConcreteVariable[G], Int] = ???
}
