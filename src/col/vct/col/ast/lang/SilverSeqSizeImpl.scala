package vct.col.ast.lang

import vct.col.ast.{SilverSeqSize, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverSeqSizeImpl[G] {
  this: SilverSeqSize[G] =>
  override def t: TInt[G] = TInt[G]()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("|") <> seq <> "|"
}
