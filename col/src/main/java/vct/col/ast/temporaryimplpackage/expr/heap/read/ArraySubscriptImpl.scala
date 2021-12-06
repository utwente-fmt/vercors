package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{ArraySubscript, Type}

trait ArraySubscriptImpl { this: ArraySubscript =>
  override def t: Type = arr.t.asArray.get.element
}