package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibSeq, Type, Z3SeqEmpty}
import vct.col.print._

trait Z3SeqEmptyImpl[G] {
  this: Z3SeqEmpty[G] =>
  override def t: Type[G] = TSmtlibSeq(elementType)
  // def layout(implicit ctx: Ctx): Doc = ???
}
