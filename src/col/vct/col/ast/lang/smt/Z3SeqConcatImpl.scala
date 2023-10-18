package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqConcat
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqConcatImpl[G] {
  this: Z3SeqConcat[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
