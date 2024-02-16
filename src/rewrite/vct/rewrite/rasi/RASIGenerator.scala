package vct.rewrite.rasi

import vct.col.ast.{BooleanValue, Expr, Or}
import vct.rewrite.cfg.CFGNode

case class RASIGenerator[G]() {
  private def neutral_element: Expr[G] = ???

  private def explore(node: CFGNode[G]): Set[AbstractState[G]] = ???

  def generate_rasi(node: CFGNode[G]): Expr[G] = {
    explore(node).map(a => a.to_expression()).fold(neutral_element)((e1, e2) => Or(e1, e2)(e1.o))
  }
}
