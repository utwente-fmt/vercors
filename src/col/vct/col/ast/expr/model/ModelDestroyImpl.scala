package vct.col.ast.expr.model

import vct.col.ast.{ModelDestroy, TVoid, Type}

trait ModelDestroyImpl[G] { this: ModelDestroy[G] =>
  override def t: Type[G] = TVoid()
}