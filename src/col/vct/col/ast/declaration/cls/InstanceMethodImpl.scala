package vct.col.ast.declaration.cls

import vct.col.ast.InstanceMethod
import vct.col.ast.declaration.category.AbstractMethodImpl
import vct.col.print._

import scala.collection.immutable.ListMap

trait InstanceMethodImpl[G] extends ClassDeclarationImpl[G] with AbstractMethodImpl[G] { this: InstanceMethod[G] =>
  def layoutModifiers(implicit ctx: Ctx): Seq[Doc] = ListMap(
    pure -> "pure",
    inline -> "inline",
  ).filter(_._1).values.map(Text).map(Doc.inlineSpec).toSeq

  override def layout(implicit ctx: Ctx): Doc =
    contract.show <+/>
    Doc.rspread(layoutModifiers) <> returnType <+> ctx.name(this) <>
      (if(typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs.map(ctx.name).map(Text)) else Empty) <>
      "(" <> Doc.args(args) <> ")" <>
      (if(outArgs.nonEmpty) Text(" returns") <+> "(" <> Doc.args(outArgs) <> ")" else Empty) <>
      body.map(Text(" ") <> _.layoutAsBlock).getOrElse(Text(";"))
}