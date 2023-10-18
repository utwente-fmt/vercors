package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqFoldlI
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqFoldlIImpl[G] {
  this: Z3SeqFoldlI[G] =>
  override def t: Type[G] = seq.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
