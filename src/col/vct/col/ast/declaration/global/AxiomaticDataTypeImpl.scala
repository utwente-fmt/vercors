package vct.col.ast.declaration.global

import vct.col.ast.util.Declarator
import vct.col.ast.{AxiomaticDataType, Declaration}
import vct.col.print._
import vct.col.ast.ops.AxiomaticDataTypeOps

trait AxiomaticDataTypeImpl[G]
    extends Declarator[G] with AxiomaticDataTypeOps[G] {
  this: AxiomaticDataType[G] =>
  override def declarations: Seq[Declaration[G]] = decls ++ typeArgs

  def layoutIsar(implicit ctx: Ctx): Doc = {
    Group(
      Group(
        // TODO(edoput) cannot use the identifiers open and close for the type notation
        // TODO(edoput) Why is Empty not in scope?
        Text("typedecl") <+>
          (if (typeArgs.nonEmpty) Text("(") <>
          // TODO(edoput) how do I map over only the types and not include also the names?
          Doc.args(typeArgs.map(ctx.name).map(Text)) <>
          Text(")") else Empty) <+> ctx.name(this)
      )
      <+/>
      Group(
        Text("locale") <+>
          ctx.name(this) <> "_signature" <+> "=" <+/>
          Doc.stack(decls) <+/>
          "begin" <+/> "end"
      )
    )
  }

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(
      Text("domain") <+> ctx.name(this) <>
        (if (typeArgs.nonEmpty)
           Text("[") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> "]"
         else
           Empty) <+> "{"
    ) <>> { Doc.stack(decls) } <+/> "}"

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(
      Text("adt") <+> ctx.name(this) <>
        (if (typeArgs.nonEmpty)
           Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">"
         else
           Empty) <+> "{"
    ) <>> { Doc.stack(decls) } <+/> "}"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case Ctx.Isar => layoutIsar
      case _ => Doc.spec(Show.lazily(layoutSpec(_)))
    }
}
