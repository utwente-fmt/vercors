package vct.col.ast.expr.op.collection

import vct.col.ast.{Size, TInt, Type}

trait SizeImpl[G] { this: Size[G] =>
  override def t: Type[G] = TInt()
}