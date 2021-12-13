package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{RemoveAt, Type}

trait RemoveAtImpl[G] { this: RemoveAt[G] =>
  override def t: Type[G] = xs.t
}