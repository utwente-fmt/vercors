package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Fork}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.ForkOps

trait ForkImpl[G] extends ForkOps[G] {
  this: Fork[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("fork") <+> obj <> ";"

  def layoutJava(implicit ctx: Ctx): Doc = obj.show <> ".start()" <> ";"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case Ctx.PVL => layoutSpec
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.obj
}
