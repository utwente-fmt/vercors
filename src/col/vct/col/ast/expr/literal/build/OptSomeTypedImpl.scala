package vct.col.ast.expr.literal.build

import vct.col.ast.{OptSomeTyped, TOption, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.OptSomeTypedOps

trait OptSomeTypedImpl[G] extends OptSomeTypedOps[G] {
  this: OptSomeTyped[G] =>
  override def t: Type[G] = TOption(element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Text("(Some ") <> e <> ")"
      case _ => Text("Some(") <> e <> ")"
    }

  }
}
