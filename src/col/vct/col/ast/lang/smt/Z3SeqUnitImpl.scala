package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibSeq, Type, Z3SeqUnit}
import vct.col.print._

trait Z3SeqUnitImpl[G] {
  this: Z3SeqUnit[G] =>
  override def t: Type[G] = TSmtlibSeq(arg.t)
  // def layout(implicit ctx: Ctx): Doc = ???
}
