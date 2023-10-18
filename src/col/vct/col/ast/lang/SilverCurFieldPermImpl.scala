package vct.col.ast.lang

import vct.col.ast.{SilverCurFieldPerm, TRational, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverCurFieldPermImpl[G] {
  this: SilverCurFieldPerm[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("perm(") <> obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field) <>
      ")"
}
