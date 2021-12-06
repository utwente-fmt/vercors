package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{RemoveAt, Type}

trait RemoveAtImpl { this: RemoveAt =>
  override def t: Type = xs.t
}