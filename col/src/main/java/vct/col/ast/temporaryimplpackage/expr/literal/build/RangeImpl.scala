package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{Range, TInt, TSeq, Type}

trait RangeImpl { this: Range =>
  override def t: Type = TSeq(TInt())
}