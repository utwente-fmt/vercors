package vct.col.ast.statement.veymont

import vct.col.ast.{Communicate, Endpoint, EndpointName, Type}
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ref.Ref
import vct.col.ast.ops.CommunicateOps

trait CommunicateImpl[G] extends CommunicateOps[G] { this: Communicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("communicate ???") // <+> receiver.show <+> "<-" <+> sender.show <> ";" // TODO

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
