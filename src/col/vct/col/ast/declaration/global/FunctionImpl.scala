package vct.col.ast.declaration.global

import vct.col.ast.Function
import vct.col.ast.declaration.category.AbstractFunctionImpl
import vct.col.print._

import scala.collection.immutable.ListMap
import vct.col.ast.ops.FunctionOps

trait FunctionImpl[G]
    extends GlobalDeclarationImpl[G]
    with AbstractFunctionImpl[G]
    with FunctionOps[G] {
  this: Function[G] =>
  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(
      Text("function") <+> ctx.name(this) <> "(" <> Doc.args(args) <> "):" <+>
        returnType
    ) <+/> contract <+/>
      (if (body.nonEmpty)
         Text("{") <>> body.get <+/> "}"
       else
         Empty)

  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] =
    ListMap(doInline -> "inline", threadLocal -> "thread_local").filter(_._1)
      .values.map(Text).map(Doc.inlineSpec).toSeq

  def layoutSpec(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Group(
          Doc.rspread(layoutModifiers) <> "pure" <+> returnType <+>
            ctx.name(this) <>
            (if (typeArgs.nonEmpty)
               Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) <> ">"
             else
               Empty) <> "(" <> Doc.args(args) <> ")"
        ) <> body.map(Text(" =") <>> _ <> ";").getOrElse(Text(";"))
      ),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.spec(Show.lazily(layoutSpec(_)))
    }
}
