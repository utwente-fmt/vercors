package vct.col.ast.expr.model

import vct.col.ast.{ModelChoose, TVoid, Type}

trait ModelChooseImpl[G] { this: ModelChoose[G] =>
  override def t: Type[G] = TVoid()
}