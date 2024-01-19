package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqAt
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.Z3SeqAtOps

trait Z3SeqAtImpl[G] extends Z3SeqAtOps[G] { this: Z3SeqAt[G] =>
  override def t: Type[G] = seq.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
