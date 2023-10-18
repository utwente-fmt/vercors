package vct.col.ast.lang

import vct.col.ast.{SilverCurPredPerm, TRational, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text, Group}

trait SilverCurPredPermImpl[G] {
  this: SilverCurPredPerm[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("perm(") <> ctx.name(ref) <> "(" <> Doc.args(args) <> ")" <> ")")
}
