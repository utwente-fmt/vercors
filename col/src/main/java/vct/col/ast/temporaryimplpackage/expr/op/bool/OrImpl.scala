package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{Or, TBool, Type}

trait OrImpl { this: Or =>
  override def t: Type = TBool()
}