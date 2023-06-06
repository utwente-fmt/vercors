package vct.col.ast.expr.literal.build

import vct.col.ast.{OptSomeTyped, TOption, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait OptSomeTypedImpl[G] { this: OptSomeTyped[G] =>
  override def t: Type[G] = TOption(element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = Text("Some(") <> e <> ")"
}