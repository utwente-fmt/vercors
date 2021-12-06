package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{PermPointer, TResource, Type}

trait PermPointerImpl { this: PermPointer =>
  override def t: Type = TResource()
}