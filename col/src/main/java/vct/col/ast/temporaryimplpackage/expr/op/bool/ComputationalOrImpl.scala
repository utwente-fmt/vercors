package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{ComputationalOr, TBool, Type}

trait ComputationalOrImpl { this: ComputationalOr =>
  override def t: Type = TBool()
}