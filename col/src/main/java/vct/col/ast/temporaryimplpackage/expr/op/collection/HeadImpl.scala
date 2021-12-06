package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Head, Type}

trait HeadImpl { this: Head =>
  override def t: Type = xs.t.asSeq.get.element
}