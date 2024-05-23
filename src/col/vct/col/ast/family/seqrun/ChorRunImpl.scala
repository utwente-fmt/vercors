package vct.col.ast.family.seqrun

import vct.col.ast.ChorRun
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{ChorRunOps, ChorRunFamilyOps}
import vct.col.ast.ops.{ChorRunOps, ChorRunFamilyOps}

trait ChorRunImpl[G] extends ChorRunOps[G] with ChorRunFamilyOps[G] { this: ChorRun[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    contract.show </> Text("seq_run") <+> this.body.show
  }
}
