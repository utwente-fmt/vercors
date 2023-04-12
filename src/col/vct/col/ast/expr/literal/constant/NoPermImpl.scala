package vct.col.ast.expr.literal.constant

import vct.col.ast.{NoPerm, TBoundedInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait NoPermImpl[G] { this: NoPerm[G] =>
  override def t: Type[G] = TBoundedInt(0, 1)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("none")
}