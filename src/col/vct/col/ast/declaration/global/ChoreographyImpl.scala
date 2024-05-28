package vct.col.ast.declaration.global

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{
  Assign,
  ChorStatement,
  Class,
  Declaration,
  Endpoint,
  EndpointGuard,
  EndpointName,
  Node,
  Choreography,
}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.Origin
import vct.col.print._
import vct.col.ref.Ref

import scala.collection.immutable.ListSet
import vct.col.ast.ops.ChoreographyOps

object ChoreographyImpl {
  def participants[G](node: Node[G]): ListSet[Endpoint[G]] =
    ListSet.from(node.collect {
      case EndpointGuard(Ref(endpoint), _) => endpoint
      case ChorStatement(Some(Ref(endpoint)), Assign(_, _)) => endpoint
      case EndpointName(Ref(endpoint)) => endpoint
    })
}

trait ChoreographyImpl[G]
    extends DeclarationImpl[G] with Declarator[G] with ChoreographyOps[G] {
  this: Choreography[G] =>
  override def declarations: Seq[Declaration[G]] = params ++ endpoints ++ decls

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(params) <> ")"
      ) <+> "{" <>> Doc.stack(
        endpoints ++ decls :+
          preRun.map(preRun => Text("/* preRun */") <+> preRun.show)
            .getOrElse(Empty) :+ run
      ) <+/> "}",
    ))

  override def enterCheckContextCurrentParticipatingEndpoints(
      context: CheckContext[G]
  ): Option[Set[Endpoint[G]]] =
    context.withCurrentParticipatingEndpoints(endpoints)

  override def enterCheckContextCurrentChoreography(
      context: CheckContext[G]
  ): Option[Choreography[G]] = Some(this)
}
