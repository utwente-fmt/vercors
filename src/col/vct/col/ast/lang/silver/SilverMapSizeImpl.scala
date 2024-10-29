package vct.col.ast.lang.silver

import vct.col.ast.{SilverMapSize, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverMapSizeOps

trait SilverMapSizeImpl[G] extends SilverMapSizeOps[G] {
  this: SilverMapSize[G] =>
  override def t: TInt[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("|") <> map.show <> "|"
}
