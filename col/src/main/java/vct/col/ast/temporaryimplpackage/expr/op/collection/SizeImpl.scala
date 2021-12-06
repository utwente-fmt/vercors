package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Size, TInt, Type}

trait SizeImpl { this: Size =>
  override def t: Type = TInt()
}