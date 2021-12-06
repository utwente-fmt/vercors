package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{ComputationalAnd, TBool, Type}

trait ComputationalAndImpl { this: ComputationalAnd =>
  override def t: Type = TBool()
}