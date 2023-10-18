package vct.col.ast.expr.op.collection

import vct.col.ast.{StringConcat, TString, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait StringConcatImpl[G] {
  this: StringConcat[G] =>
  override def t: Type[G] = TString()

  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "+", right)
}
