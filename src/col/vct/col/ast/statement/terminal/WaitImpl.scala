package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Wait}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.WaitOps

trait WaitImpl[G] extends WaitOps[G] {
  this: Wait[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("wait") <+> obj <> ";"

  def layoutJava(implicit ctx: Ctx): Doc =
    // Use signal/await as notify/wait does not work for extrinsic locks
    Text("try") <+> "{" <+> obj.show <> ".condition$" <> ".await();" <+> "}" <+>
      "catch" <+> "(" <> "InterruptedException" <+> "e" <> ")" <+> "{" <+> "}"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.obj
}
