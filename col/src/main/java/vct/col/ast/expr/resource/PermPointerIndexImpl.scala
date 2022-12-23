package vct.col.ast.expr.resource

import vct.col.ast.{PermPointerIndex, TResource, Type}

trait PermPointerIndexImpl[G] { this: PermPointerIndex[G] =>
  override def t: Type[G] = TResource()
}