package vct.col.ast.family.seqrun

import vct.col.ast.SeqRun
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{SeqRunOps, SeqRunFamilyOps}

trait SeqRunImpl[G] extends SeqRunOps[G] with SeqRunFamilyOps[G] { this: SeqRun[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    contract.show </> Text("seq_run") <+> this.body.show
  }
}
