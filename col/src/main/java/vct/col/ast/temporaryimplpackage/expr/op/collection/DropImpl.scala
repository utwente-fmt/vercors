package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Drop, Type}

trait DropImpl[G] { this: Drop[G] =>
  override def t: Type[G] = xs.t
}