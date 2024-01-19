package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqConcat
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3SeqConcatOps

trait Z3SeqConcatImpl[G] extends Z3SeqConcatOps[G] { this: Z3SeqConcat[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
