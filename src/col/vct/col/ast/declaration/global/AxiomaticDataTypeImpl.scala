package vct.col.ast.declaration.global

import vct.col.ast.util.Declarator
import vct.col.ast.{AxiomaticDataType, Declaration}
import vct.col.print._

trait AxiomaticDataTypeImpl[G] extends Declarator[G] { this: AxiomaticDataType[G] =>
  override def declarations: Seq[Declaration[G]] = decls ++ typeArgs

  def layoutSilver(implicit ctx: Ctx): Doc =
    Text("domain") <+> ctx.name(this) <>
      (if(typeArgs.nonEmpty) Text("[") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> "]" else Empty) <+>
      "{" <>>
      { Doc.stack(declarations) } <+/>
    "}"

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("adt") <+> ctx.name(this) <>
      (if(typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">" else Empty) <+>
      "{") <>>
      { Doc.stack(declarations) } <+/>
    "}"

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => Doc.spec(Show.lazily(layoutSpec(_)))
  }
}