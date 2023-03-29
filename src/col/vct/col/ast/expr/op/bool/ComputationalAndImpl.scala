package vct.col.ast.expr.op.bool

import vct.col.ast.{ComputationalAnd, TBool, Type}

trait ComputationalAndImpl[G] { this: ComputationalAnd[G] =>
  override def t: Type[G] = TBool()
}