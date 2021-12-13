package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{Div, TRational, Type}

trait DivImpl[G] { this: Div[G] =>
  override def t: Type[G] = TRational()
}