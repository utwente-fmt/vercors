package vct.col.ast.temporaryimplpackage.expr.heap.alloc

import vct.col.ast.{NewObject, TClass, Type}

trait NewObjectImpl { this: NewObject =>
  override def t: Type = TClass(cls)
}