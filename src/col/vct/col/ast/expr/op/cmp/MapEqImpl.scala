package vct.col.ast.expr.op.cmp

import vct.col.ast.{MapEq, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}

trait MapEqImpl[G] { this: MapEq[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(left) <> ".equals(" <> Doc.arg(right) <> ")")
}
