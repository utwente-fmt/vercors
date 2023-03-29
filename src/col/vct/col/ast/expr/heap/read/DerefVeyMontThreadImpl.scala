package vct.col.ast.expr.heap.read

import vct.col.ast.{DerefVeyMontThread, Type}

trait DerefVeyMontThreadImpl[G] {
  this: DerefVeyMontThread[G] =>
  override def t: Type[G] = ref.decl.threadType
}
