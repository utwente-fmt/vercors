package vct.col.ast.statement.terminal

import vct.col.ast.FramedProof
import vct.col.print.{Ctx, Doc, DocUtil, Show, Text}

trait FramedProofImpl[G] {
  this: FramedProof[G] =>
  def frameHeader(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Text("frame"),
      DocUtil.clauses("requires", pre),
      DocUtil.clauses("ensures", post),
      Text("{"),
    ))

  def layoutWithSpec(implicit ctx: Ctx): Doc =
    Doc.spec(Show.lazily(frameHeader(_))) <+/>
      Doc.stack(body.blockElementsForLayout) <+/> Doc.inlineSpec(Text("}"))

  def layoutNative(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Text("frame"),
      DocUtil.clauses("requires", pre),
      DocUtil.clauses("ensures", post),
    )) <+> body.layoutAsBlock

  override def layout(implicit ctx: Ctx): Doc =
    if (ctx.syntax != Ctx.PVL && !ctx.inSpec)
      layoutWithSpec
    else
      layoutNative
}
