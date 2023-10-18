package vct.col.ast.expr.literal.build

import vct.col.ast.{EitherLeft, TEither, TNothing, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait EitherLeftImpl[G] {
  this: EitherLeft[G] =>
  override def t: Type[G] = TEither(e.t, TNothing())

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("Left(") <> e <> ")"
}
