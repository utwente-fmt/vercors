package vct.col.ast.expr.op.option

import vct.col.ast.{OptGet, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait OptGetImpl[G] {
  this: OptGet[G] =>
  override def t: Type[G] = opt.t.asOption.get.element

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(opt) <> ".get"
}
