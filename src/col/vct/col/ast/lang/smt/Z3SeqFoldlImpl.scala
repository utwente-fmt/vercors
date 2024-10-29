package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqFoldl
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3SeqFoldlOps

trait Z3SeqFoldlImpl[G] extends Z3SeqFoldlOps[G] {
  this: Z3SeqFoldl[G] =>
  override def t: Type[G] = seq.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
