package vct.col.ast.declaration.global

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{
  Assign,
  ChorStatement,
  Choreography,
  Class,
  CommunicateTarget,
  Declaration,
  Endpoint,
  EndpointStatement,
  Node,
}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError, ChorNonTrivialContextEverywhere}
import vct.col.origin.Origin
import vct.col.print._
import vct.col.ref.Ref

import scala.collection.immutable.ListSet
import vct.col.ast.ops.ChoreographyOps
import vct.col.util.AstBuildHelpers.tt
import vct.col.util.AstMatchHelpers.{EndpointIndex, EndpointName, EndpointRange}
import vct.result.VerificationError.UserError

object ChoreographyImpl {
  case class OnlySingleEndpointsSupported(node: Node[_]) extends UserError {
    override def code: String = "onlySingleEndpointsSupported"
    override def text: String =
      node.o.messageInContext(
        "Only singular endpoint definitions are supported for permission generation"
      )
  }

  // This method is _bad_ because it collects participants from a node in an unprincipled way.
  // Instead, there should be a series of methods like this, that define it for nodes where it makes sense
  // E.g. for a choreographies, because you can look at the endpoints directly.
  // Maybe not for statements, because of unpointed expressions
  // If you assume there are no unpointed expressions, and all other endpoint owner annotations are also explicit, maybe
  //    it can make sense to define this method.
  def participants[G](node: Node[G]): ListSet[Endpoint[G]] =
    ListSet.from(node.collect {
      case EndpointName(Ref(endpoint)) => Seq(endpoint)
      case node @ EndpointRange(_, _) =>
        throw OnlySingleEndpointsSupported(node) // Seq(endpoint)
      case node @ EndpointIndex(_, _) =>
        throw OnlySingleEndpointsSupported(node)
      // Seq(endpoint)
//      case c @ ChorStatement(_) => c.participants.toSeq
    }.flatten)
}

trait ChoreographyImpl[G]
    extends DeclarationImpl[G] with Declarator[G] with ChoreographyOps[G] {
  this: Choreography[G] =>
  override def declarations: Seq[Declaration[G]] = params ++ endpoints ++ decls

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Text("choreography") <+> ctx.name(this) <> "(" <> Doc.args(params) <>
          ")"
      ) <+> "{" <>> Doc.fold(
        endpoints ++ decls :+
          preRun.map(preRun => Text("/* preRun */") <+> preRun.show)
            .getOrElse(Empty) :+ run
      )(_ <> Line <> Line <> _) <+/> "}",
    ))

  override def enterCheckContextCurrentParticipatingEndpoints(
      context: CheckContext[G]
  ): Option[Set[CommunicateTarget[G]]] =
    Some(ListSet.from(endpoints.map(_.commTarget)))

  override def enterCheckContextCurrentChoreography(
      context: CheckContext[G]
  ): Option[Choreography[G]] = Some(this)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (if (contract.contextEverywhere != tt[G])
         Seq(ChorNonTrivialContextEverywhere(contract.contextEverywhere))
       else
         Seq())
}
