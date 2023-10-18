package vct.col.ast.expr.literal.build

import vct.col.ast.{EitherRight, TEither, TNothing, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait EitherRightImpl[G] {
  this: EitherRight[G] =>
  override def t: Type[G] = TEither(TNothing(), e.t)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("Right(") <> e <> ")"
}
