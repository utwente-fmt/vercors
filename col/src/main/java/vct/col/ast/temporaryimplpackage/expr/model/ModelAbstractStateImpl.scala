package vct.col.ast.temporaryimplpackage.expr.model

import vct.col.ast.{ModelAbstractState, TResource, Type}

trait ModelAbstractStateImpl { this: ModelAbstractState =>
  override def t: Type = TResource()
}