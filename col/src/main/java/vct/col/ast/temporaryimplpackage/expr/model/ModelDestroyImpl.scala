package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelDestroy, TVoid, Type}

trait ModelDestroyImpl { this: ModelDestroy =>
  override def t: Type = TVoid()
}