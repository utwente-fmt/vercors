package vct.col.ast.lang.silver

import vct.col.ast.{SilverSeqSize, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverSeqSizeOps

trait SilverSeqSizeImpl[G] extends SilverSeqSizeOps[G] {
  this: SilverSeqSize[G] =>
  override def t: TInt[G] = TInt[G]()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("|") <> seq <> "|"
}
