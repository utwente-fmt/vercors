package vct.col.ast.statement.veymont

import vct.col.ast.{Access, Communicate, EndpointName}
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ref.Ref

trait CommunicateImpl[G] { this: Communicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("communicate") <+> receiver.show <+> "<-" <+> sender.show <> ";"

  override def check(context: CheckContext[G]): Seq[CheckError] = this match {
    case Communicate(Access(name@EndpointName(Ref(receiver)), _), _) if !context.currentParticipatingEndpoints.get.contains(receiver) =>
      Seq(SeqProgParticipant(name))
    case Communicate(_, Access(name@EndpointName(Ref(sender)), _)) if !context.currentParticipatingEndpoints.get.contains(sender) =>
      Seq(SeqProgParticipant(name))
    case _ => Nil
  }
}
