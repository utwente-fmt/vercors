package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{FloorDiv, TInt, Type}

trait FloorDivImpl[G] { this: FloorDiv[G] =>
  override def t: Type[G] = TInt()
}