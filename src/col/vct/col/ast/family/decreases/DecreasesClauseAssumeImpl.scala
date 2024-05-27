package vct.col.ast.family.decreases

import vct.col.ast.DecreasesClauseAssume
import vct.col.print._
import vct.col.ast.ops.DecreasesClauseAssumeOps

trait DecreasesClauseAssumeImpl[G] extends DecreasesClauseAssumeOps[G] {
  this: DecreasesClauseAssume[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("decreases assume;")
}
