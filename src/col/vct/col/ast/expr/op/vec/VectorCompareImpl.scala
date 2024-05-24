package vct.col.ast.expr.op.vec

import vct.col.ast.{TInt, TSeq, Type, VectorCompare}
import vct.col.ast.ops.VectorCompareOps

trait VectorCompareImpl[G] extends VectorCompareOps[G] { this: VectorCompare[G] =>
  override def t: Type[G] = TSeq(TInt()) // the results are 0 or 1, mimicking TSeq(TBool())
}