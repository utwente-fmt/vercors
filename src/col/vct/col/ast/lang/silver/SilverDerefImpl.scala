package vct.col.ast.lang.silver

import vct.col.ast.{SilverDeref, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverDerefOps

trait SilverDerefImpl[G] extends SilverDerefOps[G] { this: SilverDeref[G] =>
  override def t: Type[G] = field.decl.t

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> ctx.name(field)
}