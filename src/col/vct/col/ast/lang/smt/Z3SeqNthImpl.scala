package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqNth
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3SeqNthOps

trait Z3SeqNthImpl[G] extends Z3SeqNthOps[G] {
  this: Z3SeqNth[G] =>
  override def t: Type[G] = seq.t.asSmtlibSeq.get.element
  // def layout(implicit ctx: Ctx): Doc = ???
}
