package vct.col.ast.lang

import vct.col.ast.{SilverSeqSize, TInt}

trait SilverSeqSizeImpl[G] { this: SilverSeqSize[G] =>
  override def t: TInt[G] = TInt[G]()
}
