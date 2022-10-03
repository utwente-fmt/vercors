package vct.col.ast.lang

import vct.col.ast.{SilverSetSize, TInt}

trait SilverSetSizeImpl[G] { this: SilverSetSize[G] =>
  override def t: TInt[G] = TInt[G]()
}
