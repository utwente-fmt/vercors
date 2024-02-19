package vct.rewrite.rasi

import vct.col.ast._
import vct.rewrite.cfg.{CFGEdge, CFGEntry, CFGNode, CFGTerminal}

case class AbstractProcess[G](name: String) {
  def get_next(node: CFGEntry[G], state: AbstractState[G]): Set[AbstractState[G]] = node match {
    case CFGTerminal() => Set()
    case CFGNode(n, succ) => succ.filter(e => e.condition.isEmpty || state.resolve_boolean_expression(e.condition.get).can_be_true)
                                 .flatMap (e => process_cfg_edge(e, n)).toSet
  }

  private def process_cfg_edge(edge: CFGEdge[G], ast_node: Node[G]): Set[AbstractState[G]] = ast_node match {
    // TODO: Implement!
    case Assign(target, value) => ???
    case _ => ???
  }
}
