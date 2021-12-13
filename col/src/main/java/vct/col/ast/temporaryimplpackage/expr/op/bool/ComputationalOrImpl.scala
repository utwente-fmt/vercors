package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{ComputationalOr, TBool, Type}

trait ComputationalOrImpl[G] { this: ComputationalOr[G] =>
  override def t: Type[G] = TBool()
}