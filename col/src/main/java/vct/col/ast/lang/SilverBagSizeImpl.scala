package vct.col.ast.lang

import vct.col.ast.{SilverBagSize, TInt}

trait SilverBagSizeImpl[G] { this: SilverBagSize[G] =>
  override def t: TInt[G] = TInt[G]()
}
