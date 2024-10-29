package vct.rewrite.cfg

import vct.col.ast.{Expr, Node}

import scala.collection.mutable

sealed trait CFGEntry[G] {
  def get_successors: mutable.Set[CFGEdge[G]]
}
case class CFGNode[G](ast_node: Node[G], successors: mutable.Set[CFGEdge[G]])
    extends CFGEntry[G] {
  override def get_successors: mutable.Set[CFGEdge[G]] = successors
  override def toString: String = ast_node.toInlineString
}
case class CFGTerminal[G]() extends CFGEntry[G] {
  override def get_successors: mutable.Set[CFGEdge[G]] = mutable.Set()
  override def toString: String = "<TERMINATE>"
}

case class CFGEdge[G](target: CFGEntry[G], condition: Option[Expr[G]])
