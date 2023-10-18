package vct.col.ast.expr.resource

import vct.col.ast.{CurPerm, TRational, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait CurPermImpl[G] {
  this: CurPerm[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("perm(") <> Doc.arg(loc) <> ")")
}
