package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelCreate, TVoid, Type}

trait ModelCreateImpl { this: ModelCreate =>
  override def t: Type = TVoid()
}