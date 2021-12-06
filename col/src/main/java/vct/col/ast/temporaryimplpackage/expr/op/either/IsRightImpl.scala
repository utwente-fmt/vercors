package vct.col.ast.temporaryimplpackage.expr.op.either

import vct.col.ast.{IsRight, TBool, Type}

trait IsRightImpl { this: IsRight =>
  override def t: Type = TBool()
}