package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Take, Type}

trait TakeImpl { this: Take =>
  override def t: Type = xs.t
}