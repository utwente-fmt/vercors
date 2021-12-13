package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{TInt, TSeq, Type, VectorCompare}

trait VectorCompareImpl[G] { this: VectorCompare[G] =>
  override def t: Type[G] = TSeq(TInt()) // the results are 0 or 1, mimicking TSeq(TBool())
}