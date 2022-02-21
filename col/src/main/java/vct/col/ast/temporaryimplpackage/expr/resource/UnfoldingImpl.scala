package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.temporaryimplpackage.node.NodeFamilyImpl
import vct.col.ast.temporaryimplpackage.util.CheckFoldUnfoldTarget
import vct.col.ast.{Type, Unfolding}

trait UnfoldingImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Unfolding[G] =>
  override def t: Type[G] = body.t
}