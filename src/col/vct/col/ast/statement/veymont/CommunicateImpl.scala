package vct.col.ast.statement.veymont

import vct.col.ast.{Communicate, Endpoint, EndpointName, Type}
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ref.Ref
import vct.col.ast.ops.CommunicateOps

trait CommunicateImpl[G] extends CommunicateOps[G] { this: Communicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("communicate") <+> layoutReceiver <> target.show <+> "<-" <+> layoutSender <> msg.show <> ";")

  def layoutSender(implicit ctx: Ctx) = sender.map(_.show <> ": ").getOrElse(Text(""))
  def layoutReceiver(implicit ctx: Ctx) = receiver.map(_.show <> ": ").getOrElse(Text(""))

  override def check(context: CheckContext[G]): Seq[CheckError] = this match {
    case comm: Communicate[G] if sender.isDefined && !context.currentParticipatingEndpoints.get.contains(sender.get.ref.decl) =>
      Seq(SeqProgParticipant(sender.get))
    case comm: Communicate[G] if receiver.isDefined && !context.currentParticipatingEndpoints.get.contains(receiver.get.ref.decl) =>
      Seq(SeqProgParticipant(receiver.get))
    case _ => Nil
  }

  def participants: Seq[Endpoint[G]] =
    (sender.toSeq ++ receiver.toSeq).map(_.ref.decl)
}
