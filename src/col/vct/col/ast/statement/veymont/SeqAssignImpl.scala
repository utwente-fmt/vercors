package vct.col.ast.statement.veymont

import vct.col.ast.SeqAssign
import vct.col.ast.statement.StatementImpl
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SeqAssignOps

trait SeqAssignImpl[G] extends StatementImpl[G] with SeqAssignOps[G] { this: SeqAssign[G] =>

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
      context.withReceiverEndpoint(receiver.decl)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(ctx.name(receiver)) <> "." <> ctx.name(field) <+> ":=" <+> value.show)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if (!context.currentParticipatingEndpoints.get.contains(this.receiver.decl))
      Seq(SeqProgParticipant(this))
    else Nil
}
