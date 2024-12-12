package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Join}
import vct.col.print.{Ctx, Doc, Group, Show, Text}
import vct.col.ast.ops.JoinOps

trait JoinImpl[G] extends JoinOps[G] {
  this: Join[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("join") <+> obj <> ";"

  def layoutJava(implicit ctx: Ctx): Doc =
    Group(
      Text("try") <+> "{" <>>
        (obj.show <> Text(".join()") <> ";") <+/> "}" <+> "catch" <+> "(" <>
        "InterruptedException" <+> "e" <> ")" <+> "{" <+> "}"
    )

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case Ctx.PVL => layoutSpec
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.obj
}
