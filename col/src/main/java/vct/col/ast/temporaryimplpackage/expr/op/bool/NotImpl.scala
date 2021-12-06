package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{Not, TBool, Type}

trait NotImpl { this: Not =>
  override def t: Type = TBool()
}