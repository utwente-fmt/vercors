package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapSize, TInt, Type}

trait MapSizeImpl { this: MapSize =>
  override def t: Type = TInt()
}
