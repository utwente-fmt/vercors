package vct.col.ast.lang

import vct.col.ast.LlvmSpecFunction
import vct.col.ast.declaration.category.AbstractFunctionImpl
import vct.col.ast.declaration.global.GlobalDeclarationImpl
import vct.col.print.{Ctx, Doc, Empty, Group, Show, Text}

import scala.collection.immutable.ListMap

trait LLVMSpecFunctionImpl[G]
    extends GlobalDeclarationImpl[G] with AbstractFunctionImpl[G] {
  this: LlvmSpecFunction[G] =>

  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] =
    ListMap(inline -> "inline", threadLocal -> "thread_local").filter(_._1)
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
    Doc.spec(Show.lazily(layoutSpec(_)))

}
