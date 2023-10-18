package vct.col.ast.expr.model

import vct.col.ast.{ModelState, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait ModelStateImpl[G] {
  this: ModelState[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      assoc(model) <> "." <> "state" <> "(" <> Doc.args(Seq(perm, state)) <> ")"
    )
}
