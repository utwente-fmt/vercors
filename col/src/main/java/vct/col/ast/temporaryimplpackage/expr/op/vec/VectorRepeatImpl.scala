package vct.col.ast.temporaryimplpackage.expr.op.vec

import vct.col.ast.{TSeq, Type, VectorRepeat}

trait VectorRepeatImpl[G] { this: VectorRepeat[G] =>
  override def t: Type[G] = TSeq(e.t)
}