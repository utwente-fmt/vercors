package vct.col.ast.temporaryimplpackage.expr.lock

import vct.col.ast.{IdleToken, TResource, Type}

trait IdleTokenImpl { this: IdleToken =>
  override def t: Type = TResource()
}