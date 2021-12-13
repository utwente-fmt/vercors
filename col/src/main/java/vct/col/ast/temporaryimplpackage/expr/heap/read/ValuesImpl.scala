package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{TSeq, Type, Values}

trait ValuesImpl[G] { this: Values[G] =>
  override def t: Type[G] = TSeq(arr.t.asArray.get.element)
}