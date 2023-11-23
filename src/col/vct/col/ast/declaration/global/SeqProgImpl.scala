package vct.col.ast.declaration.global

import vct.col.ast.{Class, Declaration, Endpoint, EndpointGuard, EndpointName, Node, SeqAssign, SeqProg}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.Origin
import vct.col.print._
import vct.col.ref.Ref

import scala.collection.immutable.ListSet

object SeqProgImpl {
  def participants[G](node: Node[G]): ListSet[Endpoint[G]] =
    ListSet.from(node.collect {
      case EndpointGuard(Ref(endpoint), _) => endpoint
      case SeqAssign(Ref(endpoint), _, _) => endpoint
      case EndpointName(Ref(endpoint)) => endpoint
    })
}

trait SeqProgImpl[G] extends Declarator[G] { this: SeqProg[G] =>
  override def declarations: Seq[Declaration[G]] = args ++ endpoints ++ decls

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <+> "{" <>>
        Doc.stack(endpoints ++ decls :+ run) <+/>
      "}"
    ))

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    super.enterCheckContext(context)
      .withSeqProg(this)
      .withCurrentParticipatingEndpoints(endpoints)
}
