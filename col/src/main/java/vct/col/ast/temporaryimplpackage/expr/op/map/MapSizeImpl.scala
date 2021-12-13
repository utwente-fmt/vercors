package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapSize, TInt, Type}

trait MapSizeImpl[G] { this: MapSize[G] =>
  override def t: Type[G] = TInt()
}
