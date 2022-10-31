package vct.col.ast.expr.op.collection

import vct.col.ast.{Head, Type}

trait HeadImpl[G] { this: Head[G] =>
  override def t: Type[G] = xs.t.asSeq.get.element
}