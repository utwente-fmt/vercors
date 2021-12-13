package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{ComputationalXor, TBool, Type}

trait ComputationalXorImpl[G] { this: ComputationalXor[G] =>
  override def t: Type[G] = TBool()
}