package vct.col.ast.declaration.global

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.{Class, Declaration, Endpoint, EndpointGuard, EndpointName, Node, SeqAssign, SeqProg}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.Origin
import vct.col.print._
import vct.col.ref.Ref

import scala.collection.immutable.ListSet
import vct.col.ast.ops.SeqProgOps

object SeqProgImpl {
  def participants[G](node: Node[G]): ListSet[Endpoint[G]] =
    ListSet.from(node.collect {
      case EndpointGuard(Ref(endpoint), _) => endpoint
      case SeqAssign(Ref(endpoint), _, _) => endpoint
      case EndpointName(Ref(endpoint)) => endpoint
    })
}

trait SeqProgImpl[G] extends DeclarationImpl[G] with Declarator[G] with SeqProgOps[G] { this: SeqProg[G] =>
  override def declarations: Seq[Declaration[G]] = args ++ endpoints ++ decls

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <+> "{" <>>
        Doc.stack(endpoints ++ decls :+ run) <+/>
      "}"
    ))

  override def enterCheckContextCurrentParticipatingEndpoints(context: CheckContext[G]): Option[Set[Endpoint[G]]] =
    context.withCurrentParticipatingEndpoints(endpoints)

  override def enterCheckContextCurrentSeqProg(context: CheckContext[G]): Option[SeqProg[G]] =
    Some(this)
}
