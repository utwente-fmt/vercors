package vct.rewrite.cfg

import vct.col.ast.Node

case class CFGNode[G](ast_node: Node[G], successors: scala.collection.mutable.Set[CFGNode[G]])  // TODO: Add condition information to edges