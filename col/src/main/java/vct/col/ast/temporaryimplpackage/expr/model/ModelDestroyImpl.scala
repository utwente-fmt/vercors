package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelDestroy, TVoid, Type}

trait ModelDestroyImpl[G] { this: ModelDestroy[G] =>
  override def t: Type[G] = TVoid()
}