package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{And, TBool, Type}

trait AndImpl { this: And =>
  override def t: Type = TBool()
}