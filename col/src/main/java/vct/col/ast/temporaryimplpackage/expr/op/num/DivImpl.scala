package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{Div, TRational, Type}

trait DivImpl { this: Div =>
  override def t: Type = TRational()
}