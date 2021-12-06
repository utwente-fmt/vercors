package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Slice, Type}

trait SliceImpl { this: Slice =>
  override def t: Type = xs.t
}