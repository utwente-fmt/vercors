package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Notify}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.NotifyOps

trait NotifyImpl[G] extends NotifyOps[G] {
  this: Notify[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("notify") <+> obj <> ";"

  def layoutJava(implicit ctx: Ctx): Doc =
    // Use signal/await as notify/wait does not work for extrinsic locks
    obj.show <> ".condition$" <> ".signal()" <> ";"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.obj
}
