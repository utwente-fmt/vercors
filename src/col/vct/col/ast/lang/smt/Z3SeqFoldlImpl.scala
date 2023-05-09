package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqFoldl
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqFoldlImpl[G] { this: Z3SeqFoldl[G] =>
  override def t: Type[G] = seq.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
