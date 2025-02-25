package vct.col.ast.declaration.global

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{
  Assign,
  ChorStatement,
  Choreography,
  Class,
  Declaration,
  Endpoint,
  EndpointName,
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

object ChoreographyImpl {
  def participants[G](node: Node[G]): ListSet[Endpoint[G]] =
    ListSet.from(node.collect {
      case EndpointStatement(Some(Ref(endpoint)), Assign(_, _)) => Seq(endpoint)
      case EndpointName(Ref(endpoint)) => Seq(endpoint)
      case c @ ChorStatement(_) => c.participants.toSeq
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
  ): Option[Set[Endpoint[G]]] =
    context.withCurrentParticipatingEndpoints(endpoints)

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
