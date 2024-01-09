package vct.col.ast.declaration.adt

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{ADTFunction, Node}
import vct.col.print._
import vct.col.ast.ops.ADTFunctionOps

trait ADTFunctionImpl[G] extends ApplicableImpl[G] with ADTDeclarationImpl[G] with ADTFunctionOps[G] { this: ADTFunction[G] =>
  override def body: Option[Node[G]] = None

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => Group(Text("function") <+> ctx.name(this) <> "(" <> Doc.args(args) <> "):" <+> returnType)
    case _ => Group(Text("pure") <+> returnType <+> ctx.name (this) <> "(" <> Doc.args(args) <> ");")
  }
}