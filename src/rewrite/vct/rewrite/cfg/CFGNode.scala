package vct.rewrite.cfg

import vct.col.ast.Node

case class CFGNode[G](ast_node: Node[G], successors: Set[CFGNode[G]])