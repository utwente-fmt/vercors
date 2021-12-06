package vct.col.ast.temporaryimplpackage.expr.op.bool

import vct.col.ast.{ComputationalXor, TBool, Type}

trait ComputationalXorImpl { this: ComputationalXor =>
  override def t: Type = TBool()
}