package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqExtract
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3SeqExtractOps

trait Z3SeqExtractImpl[G] extends Z3SeqExtractOps[G] {
  this: Z3SeqExtract[G] =>
  override def t: Type[G] = seq.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
