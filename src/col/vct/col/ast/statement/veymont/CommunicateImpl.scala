package vct.col.ast.statement.veymont

import hre.data.BitString
import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{
  CommTargetEndpoint,
  CommTargetRange,
  Communicate,
  CommunicateTarget,
  CtExpr,
  Endpoint,
  Expr,
  Message,
  Node,
  RangeBinder,
  TClass,
  Type,
  Variable,
}
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print.{Ctx, Doc, Group, Nest, Text}
import vct.col.ref.Ref
import vct.col.ast.ops.{CommunicateFamilyOps, CommunicateOps}
import vct.result.VerificationError.UserError

import scala.collection.immutable.{AbstractSeq, LinearSeq}

trait CommunicateImpl[G]
    extends DeclarationImpl[G]
    with CommunicateOps[G]
    with CommunicateFamilyOps[G] {
  comm: Communicate[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("channel_invariant") <+> Nest(invariant.show) <> ";" <+/> Group(
      Text("communicate") <+> receiver.map(_.show <> " ").getOrElse(Text("")) <>
        destination.show <+> "<-" <+> sender.map(_.show <> " ")
          .getOrElse(Text("")) <> msg.show <> ";"
    )

  def layoutParticipant(endpoint: Option[Ref[G, Endpoint[G]]])(
      implicit ctx: Ctx
  ) = endpoint.map(ref => Text(ctx.name(ref)) <> ": ").getOrElse(Text(""))

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    if (sender.get.isRange || receiver.get.isRange)
      Seq()
    else
      this match {
        case comm: Communicate[G]
            if sender.isDefined &&
              !context.currentParticipatingEndpoints.get.contains(sender.get) =>
          comm.sender match {
            case Some(CommTargetEndpoint(_)) =>
              Seq(SeqProgParticipant(sender.get))
            case _ => Seq() // Ignore this check for parameterized endpoint
          }
        case comm: Communicate[G]
            if receiver.isDefined &&
              !context.currentParticipatingEndpoints.get
                .contains(receiver.get) =>
          comm.receiver match {
            case Some(CommTargetEndpoint(_)) =>
              Seq(SeqProgParticipant(receiver.get))
            case _ => Seq() // Ignore this check for parameterized endpoint
          }
        case _ => Nil
      }
  }

  case class TooManyRangeBinders(ex1: Node[_], ex2: Node[_]) extends UserError {
    override def code: String = "tooManyRangeBinders"
    override def text: String =
      vct.result.Message.messagesInContext(
        (
          ex1.o,
          "Only one range binder allowed, but two detected. For example here, and...",
        ),
        (ex2.o, "... here."),
      )
  }

  def rangeBinder(node: Node[G]): Option[Variable[G]] =
    node match {
      case CommTargetRange(_, RangeBinder(v, _, _)) => Some(v)
      case e: Expr[G] =>
        e.collect { case CtExpr(CommTargetRange(_, RangeBinder(v, _, _))) =>
            v
          } match {
          case Seq(v) => Some(v)
          case Seq() => None
          case xs => throw TooManyRangeBinders(xs.head, xs(1))
        }
      case _ => None
    }

  def rangeBinders: Seq[Variable[G]] =
    receiver.flatMap(rangeBinder).toSeq ++ sender.flatMap(rangeBinder).toSeq ++
      rangeBinder(destination).toSeq ++ rangeBinder(msg).toSeq

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] = {
    val ctxRangeBinder = context.copy(scopes = context.withScope(rangeBinders))
    receiver.map(f(ctxRangeBinder, _)).toSeq ++ sender.map(f(ctxRangeBinder, _))
      .toSeq ++ Seq(
      f(context.withCommunicateInvariant(this), invariant),
      f(ctxRangeBinder, destination),
      f(ctxRangeBinder, msg),
    )
//    subnodes match {
//      case invariant +: rest =>
//        f(context.withCommunicateInvariant(this), invariant) +:
//          rest.map(f(enterCheckContext(context), _))
//      case _ => ???
//    }
  }

  def participants: Seq[CommunicateTarget[G]] = sender.toSeq ++ receiver.toSeq

  object t {
    def sender: TClass[G] = comm.sender.get.ref.decl.singleType
    def receiver: TClass[G] = comm.receiver.get.ref.decl.singleType
  }
}
