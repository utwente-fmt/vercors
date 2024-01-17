package vct.col.ast.expr.literal.constant

import vct.col.ast.{OptNone, TNothing, TOption, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.OptNoneOps

trait OptNoneImpl[G] extends OptNoneOps[G] { this: OptNone[G] =>
  override def t: Type[G] = TOption(TNothing())

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("None")
}