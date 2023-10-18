package vct.col.ast.lang

import vct.col.ast.{SilverDeref, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverDerefImpl[G] {
  this: SilverDeref[G] =>
  override def t: Type[G] = field.decl.t

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> ctx.name(field)
}
