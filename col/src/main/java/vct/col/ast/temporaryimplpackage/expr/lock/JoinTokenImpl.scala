package vct.col.ast.temporaryimplpackage.expr.lock

import vct.col.ast.{JoinToken, TResource, Type}

trait JoinTokenImpl { this: JoinToken =>
  override def t: Type = TResource()
}