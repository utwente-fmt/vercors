package vct.col.ast.lang

import vct.col.ast.{SilverMapSize, TInt}

trait SilverMapSizeImpl[G] { this: SilverMapSize[G] =>
  override def t: TInt[G] = TInt()
}
