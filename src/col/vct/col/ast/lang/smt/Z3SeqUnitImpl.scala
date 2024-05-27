package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibSeq, Type, Z3SeqUnit}
import vct.col.print._
import vct.col.ast.ops.Z3SeqUnitOps

trait Z3SeqUnitImpl[G] extends Z3SeqUnitOps[G] {
  this: Z3SeqUnit[G] =>
  override def t: Type[G] = TSmtlibSeq(arg.t)
  // def layout(implicit ctx: Ctx): Doc = ???
}
