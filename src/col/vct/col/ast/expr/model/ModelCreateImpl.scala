package vct.col.ast.expr.model

import vct.col.ast.{ModelCreate, TVoid, Type}

trait ModelCreateImpl[G] { this: ModelCreate[G] =>
  override def t: Type[G] = TVoid()
}