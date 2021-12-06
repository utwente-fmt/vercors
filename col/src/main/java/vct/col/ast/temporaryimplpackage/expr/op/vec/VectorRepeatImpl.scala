package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{TSeq, Type, VectorRepeat}

trait VectorRepeatImpl { this: VectorRepeat =>
  override def t: Type = TSeq(e.t)
}