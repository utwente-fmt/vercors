package vct.col.ast.expr.op.vec

import vct.col.ast.{TSeq, Type, VectorRepeat}
import vct.col.ast.ops.VectorRepeatOps

trait VectorRepeatImpl[G] extends VectorRepeatOps[G] {
  this: VectorRepeat[G] =>
  override def t: Type[G] = TSeq(e.t)
}
