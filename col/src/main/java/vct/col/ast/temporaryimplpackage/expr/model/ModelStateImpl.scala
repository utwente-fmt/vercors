package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelState, TResource, Type}

trait ModelStateImpl { this: ModelState =>
  override def t: Type = TResource()
}