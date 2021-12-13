package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelState, TResource, Type}

trait ModelStateImpl[G] { this: ModelState[G] =>
  override def t: Type[G] = TResource()
}