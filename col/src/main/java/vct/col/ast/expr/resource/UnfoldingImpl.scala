package vct.col.ast.expr.resource

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.util.CheckFoldUnfoldTarget
import vct.col.ast.{Type, Unfolding}

trait UnfoldingImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Unfolding[G] =>
  override def t: Type[G] = body.t
}