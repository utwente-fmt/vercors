package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqFoldlI
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3SeqFoldlIOps

trait Z3SeqFoldlIImpl[G] extends Z3SeqFoldlIOps[G] {
  this: Z3SeqFoldlI[G] =>
  override def t: Type[G] = seq.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
