package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Unlock}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.UnlockOps

trait UnlockImpl[G] extends UnlockOps[G] {
  this: Unlock[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("unlock") <+> obj <> ";"

  def layoutJava(implicit ctx: Ctx): Doc =
    obj.show <> ".intrinsicLock$.unlock();"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.obj
}
