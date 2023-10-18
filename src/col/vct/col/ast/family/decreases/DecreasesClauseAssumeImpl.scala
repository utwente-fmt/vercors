package vct.col.ast.family.decreases

import vct.col.ast.DecreasesClauseAssume
import vct.col.print._

trait DecreasesClauseAssumeImpl[G] {
  this: DecreasesClauseAssume[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("decreases assume;")
}
