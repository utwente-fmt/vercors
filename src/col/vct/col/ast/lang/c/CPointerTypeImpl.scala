package vct.col.ast.lang.c

import vct.col.ast.{CPointerType, Type}

trait CPointerTypeImpl[G] {
  this: CPointerType[G] =>

  val innerType: Type[G]
}
