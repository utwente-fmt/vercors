package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Take, Type}

trait TakeImpl[G] { this: Take[G] =>
  override def t: Type[G] = xs.t
}