package vct.col.ast.temporaryimplpackage.expr.heap.alloc

import vct.col.ast.{NewObject, TClass, Type}

trait NewObjectImpl[G] { this: NewObject[G] =>
  override def t: Type[G] = TClass(cls)
}