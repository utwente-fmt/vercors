package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibSeq, Type, Z3SeqEmpty}
import vct.col.print._
import vct.col.ast.ops.Z3SeqEmptyOps

trait Z3SeqEmptyImpl[G] extends Z3SeqEmptyOps[G] { this: Z3SeqEmpty[G] =>
  override def t: Type[G] = TSmtlibSeq(elementType)
  // def layout(implicit ctx: Ctx): Doc = ???
}
