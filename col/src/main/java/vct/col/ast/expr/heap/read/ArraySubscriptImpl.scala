package vct.col.ast.expr.heap.read

import vct.col.ast.{ArraySubscript, Type}

trait ArraySubscriptImpl[G] { this: ArraySubscript[G] =>
  override def t: Type[G] = arr.t.asArray.get.element
}