package vct.col.ast.family.seqrun

import vct.col.ast.SeqRun
import vct.col.print.{Ctx, Doc, Text}

trait SeqRunImpl[G] { this: SeqRun[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    contract.show </> Text("seq_run") <+> this.body.show
  }
}
