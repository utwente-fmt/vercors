package vct.col.ast.family.loopcontract

import vct.col.ast.IterationContract
import vct.col.print._

trait IterationContractImpl[G] {
  this: IterationContract[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      DocUtil.clauses("context_everywhere", context_everywhere),
      DocUtil.clauses("requires", requires),
      DocUtil.clauses("ensures", ensures),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    Doc.spec(Show.lazily(layoutSpec(_)))
}
