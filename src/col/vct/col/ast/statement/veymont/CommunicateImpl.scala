package vct.col.ast.statement.veymont

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{Communicate, Endpoint, Node, TClass, Type}
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print.{Ctx, Doc, Group, Nest, Text}
import vct.col.ref.Ref
import vct.col.ast.ops.{CommunicateFamilyOps, CommunicateOps}

trait CommunicateImpl[G]
    extends DeclarationImpl[G]
    with CommunicateOps[G]
    with CommunicateFamilyOps[G] {
  comm: Communicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("channel_invariant") <+> Nest(invariant.show) <> ";" <+/> Group(
      Text("communicate") <+> receiver.map(_.show).getOrElse(Text("")) <>
        destination.show <+> "<-" <+> sender.map(_.show).getOrElse(Text("")) <>
        msg.show <> ";"
    )

  def layoutParticipant(endpoint: Option[Ref[G, Endpoint[G]]])(
      implicit ctx: Ctx
  ) = endpoint.map(ref => Text(ctx.name(ref)) <> ": ").getOrElse(Text(""))

  override def check(context: CheckContext[G]): Seq[CheckError] =
    this match {
      case comm: Communicate[G]
          if sender.isDefined &&
            !context.currentParticipatingEndpoints.get
              .contains(??? /* TODO (RR): sender.get.decl */ ) =>
        Seq(SeqProgParticipant(??? /* TODO (RR): sender.get.decl */ ))
      case comm: Communicate[G]
          if receiver.isDefined &&
            !context.currentParticipatingEndpoints.get
              .contains(??? /* TODO (RR): receiver.get.decl */ ) =>
        Seq(SeqProgParticipant(??? /* TODO (RR): receiver.get.decl */ ))
      case _ => Nil
    }

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] =
    subnodes match {
      case invariant +: rest =>
        f(context.withCommunicateInvariant(this), invariant) +:
          rest.map(f(enterCheckContext(context), _))
      case _ => ???
    }

  def participants: Seq[Endpoint[G]] = (sender.toSeq ++ receiver.toSeq)
    .map(_.endpoint)

  object t {
    def sender: TClass[G] = comm.sender.get.endpoint.t
    def receiver: TClass[G] = comm.receiver.get.endpoint.t
  }
}
