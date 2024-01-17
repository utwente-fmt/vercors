package vct.col.ast.statement.composite

import vct.col.ast.ParAtomic
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.ParAtomicOps

trait ParAtomicImpl[G] extends ParAtomicOps[G] { this: ParAtomic[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    inv.flatMap(context.checkInScope(this, _))

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("atomic(") <> Doc.args(inv.map(ctx.name).map(Text)) <> ")") <+> content.layoutAsBlock
}