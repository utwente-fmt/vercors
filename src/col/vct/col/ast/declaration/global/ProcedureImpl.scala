package vct.col.ast.declaration.global

import vct.col.ast.Procedure
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.ProcedureOps

trait ProcedureImpl[G] extends ProcedureOps[G] { this: Procedure[G] =>
  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text("method") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")" <>
      (if(outArgs.nonEmpty) Text(" returns (") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ")" else Empty)) <+/>
      contract <>
      (if(body.nonEmpty) Line <> body.get.layoutAsBlock else Empty)

  def layoutC(implicit ctx: Ctx): Doc = {
    val (spec, decl) = returnType.layoutSplitDeclarator
    Group(spec <+> decl <> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <>
      (if(body.nonEmpty) Text(" ") <> body.get.layoutAsBlock else Text(";"))
  }

  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] = ListMap(
    pure -> "pure",
    inline -> "inline",
  ).filter(_._1).values.map(Text).map(Doc.inlineSpec).toSeq

  def layoutSpec(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Group(Doc.rspread(layoutModifiers) <> returnType <+> ctx.name(this) <>
        (if (typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">" else Empty)) <>
        "(" <> Doc.args(args) <> ")" <>
        (if (outArgs.nonEmpty) Text(" returns") <+> "(" <> Doc.args(outArgs) <> ")" else Empty)) <>
        body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";")),
    ))

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP => layoutC
    case Ctx.PVL | Ctx.Java => Doc.spec(Show.lazily(layoutSpec(_)))
  }
}