package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Drop, Type}

trait DropImpl { this: Drop =>
  override def t: Type = xs.t
}