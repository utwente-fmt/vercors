package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{TSeq, Type, Values}

trait ValuesImpl { this: Values =>
  override def t: Type = TSeq(arr.t.asArray.get.element)
}