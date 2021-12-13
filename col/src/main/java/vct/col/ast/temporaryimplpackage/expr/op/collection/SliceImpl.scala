package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Slice, Type}

trait SliceImpl[G] { this: Slice[G] =>
  override def t: Type[G] = xs.t
}