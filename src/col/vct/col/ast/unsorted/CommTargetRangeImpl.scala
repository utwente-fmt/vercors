package vct.col.ast.unsorted

import vct.col.ast.CommTargetRange
import vct.col.print._
import vct.col.ast.ops.CommTargetRangeOps
import vct.col.check.{CheckContext, CheckError}

trait CommTargetRangeImpl[G]
    extends CommTargetRangeOps[G] with CommunicateTargetImpl[G] {
  this: CommTargetRange[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref)) <> range

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ {
      // This is really a well-formedness requirement on the AST. Ideally it would be checked earlier, but this is not
      // possibly, as the ref might not be resolved yet.
      require(ref.decl.isFamily)
      Seq()
    }
}
