package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Tail, Type}

trait TailImpl[G] { this: Tail[G] =>
  override def t: Type[G] = xs.t
}