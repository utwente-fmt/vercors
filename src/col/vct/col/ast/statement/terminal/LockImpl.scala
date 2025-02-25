package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Lock}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.LockOps

trait LockImpl[G] extends LockOps[G] {
  this: Lock[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("lock") <+> obj <> ";"

  def layoutJava(implicit ctx: Ctx): Doc = obj.show <> ".intrinsicLock$.lock();"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.obj
}
