package vct.col.ast.temporaryimplpackage.expr.op.either

import vct.col.ast.{IsLeft, TBool, Type}

trait IsLeftImpl { this: IsLeft =>
  override def t: Type = TBool()
}