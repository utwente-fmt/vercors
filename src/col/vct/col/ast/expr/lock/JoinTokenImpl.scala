package vct.col.ast.expr.lock

import vct.col.ast.{JoinToken, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait JoinTokenImpl[G] {
  this: JoinToken[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("running(") <> Doc.arg(thread) <> ")")
}
