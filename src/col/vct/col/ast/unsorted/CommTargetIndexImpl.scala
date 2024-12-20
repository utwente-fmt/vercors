package vct.col.ast.unsorted

import vct.col.ast.CommTargetIndex
import vct.col.print._
import vct.col.ast.ops.CommTargetIndexOps
import vct.col.check.{CheckContext, CheckError}

trait CommTargetIndexImpl[G]
    extends CommTargetIndexOps[G] with CommunicateTargetImpl[G] {
  this: CommTargetIndex[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(ref)) <> "[" <> index <> "]"

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ {
      // This is really a well-formedness requirement on the AST. Ideally it would be checked earlier, but this is not
      // possibly, as the ref might not be resolved yet.
      require(ref.decl.isFamily)
      Seq()
    }
}
