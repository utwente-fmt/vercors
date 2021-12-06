package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{PermPointerIndex, TResource, Type}

trait PermPointerIndexImpl { this: PermPointerIndex =>
  override def t: Type = TResource()
}