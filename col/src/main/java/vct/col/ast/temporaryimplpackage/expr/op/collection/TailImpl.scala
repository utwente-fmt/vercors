package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Tail, Type}

trait TailImpl { this: Tail =>
  override def t: Type = xs.t
}