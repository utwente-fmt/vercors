package vct.col.ast.family.seqrun

import vct.col.ast.ChorRun
import vct.col.ast.node.NodeFamilyImpl
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{ChorRunFamilyOps, ChorRunOps}
import vct.col.ast.ops.{ChorRunFamilyOps, ChorRunOps}
import vct.col.check.{CheckContext, CheckError, ChorNonTrivialContextEverywhere}
import vct.col.util.AstBuildHelpers.tt

trait ChorRunImpl[G]
    extends NodeFamilyImpl[G] with ChorRunOps[G] with ChorRunFamilyOps[G] {
  this: ChorRun[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    contract.show </> Text("run") <+> this.body.show
  }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (if (contract.contextEverywhere != tt[G])
         Seq(ChorNonTrivialContextEverywhere(contract.contextEverywhere))
       else
         Seq())
}
