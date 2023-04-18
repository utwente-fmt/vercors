package vct.col.ast.expr.lock

import vct.col.ast.{Held, TBool, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait HeldImpl[G] { this: Held[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("held(") <> Doc.arg(obj) <> ")")
}