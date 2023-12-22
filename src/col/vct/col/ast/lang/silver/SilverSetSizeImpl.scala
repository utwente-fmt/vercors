package vct.col.ast.lang.silver

import vct.col.ast.{SilverSetSize, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverSetSizeOps

trait SilverSetSizeImpl[G] extends SilverSetSizeOps[G] { this: SilverSetSize[G] =>
  override def t: TInt[G] = TInt[G]()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("|") <> set <> "|"
}
