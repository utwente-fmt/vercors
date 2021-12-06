package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{TInt, TSeq, Type, VectorCompare}

trait VectorCompareImpl { this: VectorCompare =>
  override def t: Type = TSeq(TInt()) // the results are 0 or 1, mimicking TSeq(TBool())
}