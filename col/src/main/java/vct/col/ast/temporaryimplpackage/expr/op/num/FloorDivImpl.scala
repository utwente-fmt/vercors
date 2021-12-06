package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{FloorDiv, TInt, Type}

trait FloorDivImpl { this: FloorDiv =>
  override def t: Type = TInt()
}