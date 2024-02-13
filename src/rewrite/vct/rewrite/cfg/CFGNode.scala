package vct.rewrite.cfg

import vct.col.ast.{Expr, Node}

import scala.collection.mutable

case class CFGNode[G](ast_node: Node[G], successors: mutable.Set[CFGNode[G]])  // TODO: Add condition information to edges
case class CFGEdge[G](target: CFGNode[G], condition: Option[Expr[G]])