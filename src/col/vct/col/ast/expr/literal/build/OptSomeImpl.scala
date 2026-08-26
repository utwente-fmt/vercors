package vct.col.ast.expr.literal.build

import vct.col.ast.{OptSome, TOption, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.OptSomeOps

trait OptSomeImpl[G] extends OptSomeOps[G] {
  this: OptSome[G] =>
  override def t: Type[G] = TOption(e.t)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Text("(Some") <+> e <> ")"
      case _ => Text("Some(") <> e <> ")"
    }

  }
}
