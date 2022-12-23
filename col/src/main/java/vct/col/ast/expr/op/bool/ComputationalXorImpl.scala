package vct.col.ast.expr.op.bool

import vct.col.ast.{ComputationalXor, TBool, Type}

trait ComputationalXorImpl[G] { this: ComputationalXor[G] =>
  override def t: Type[G] = TBool()
}