package vct.col.ast.declaration.adt

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{ADTFunction, Node}
import vct.col.print._

trait ADTFunctionImpl[G] extends ApplicableImpl[G] with ADTDeclarationImpl[G] { this: ADTFunction[G] =>
  override def body: Option[Node[G]] = None

  override def layout(implicit ctx: Ctx): Doc =
    Text("pure") <+> returnType <+> ctx.name(this) <> "(" <> Doc.args(args) <> ");"
}