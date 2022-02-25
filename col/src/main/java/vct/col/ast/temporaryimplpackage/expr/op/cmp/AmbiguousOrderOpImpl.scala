package vct.col.ast.temporaryimplpackage.expr.op.cmp

import vct.col.ast.AmbiguousOrderOp

trait AmbiguousOrderOpImpl[G] { this: AmbiguousOrderOp[G] =>
  def isSetOp: Boolean = left.t.asSet.isDefined
  def isBagOp: Boolean = left.t.asBag.isDefined
}
