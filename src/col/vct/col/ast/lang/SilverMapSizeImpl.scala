package vct.col.ast.lang

import vct.col.ast.{SilverMapSize, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverMapSizeImpl[G] { this: SilverMapSize[G] =>
  override def t: TInt[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =

}
