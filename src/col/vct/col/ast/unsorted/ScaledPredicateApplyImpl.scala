package vct.col.ast.unsorted

import vct.col.ast.{ScaledPredicateApply, WritePerm}
import vct.col.ast.ops.ScaledPredicateApplyOps
import vct.col.check.{AbstractPredicate, CheckContext, CheckError}
import vct.col.print._

trait ScaledPredicateApplyImpl[G]
    extends FoldTargetImpl[G] with ScaledPredicateApplyOps[G] {
  this: ScaledPredicateApply[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    perm match {
      case WritePerm() => apply.show
      case perm => Group(Text("[") <> Doc.arg(perm) <> "]" <> apply)
    }

  override def check(ctx: CheckContext[G]): Seq[CheckError] =
    super.check(ctx) ++
      (if (apply.ref.decl.body.isEmpty)
         Seq(AbstractPredicate(this))
       else
         Nil)
}
