package vct.col.ast.expr.lock

import vct.col.ast.{IdleToken, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait IdleTokenImpl[G] { this: IdleToken[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("idle(") <>> thread </> ")")
}