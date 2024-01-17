package vct.col.ast.lang.silver

import vct.col.ast.{SilverBagSize, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverBagSizeOps

trait SilverBagSizeImpl[G] extends SilverBagSizeOps[G] { this: SilverBagSize[G] =>
  override def t: TInt[G] = TInt[G]()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("|") <> bag <> "|"
}
