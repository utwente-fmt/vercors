package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibSeq, Type, Z3SeqMapI}
import vct.col.print._

trait Z3SeqMapIImpl[G] { this: Z3SeqMapI[G] =>
  override lazy val t: Type[G] = TSmtlibSeq(f.t.asSmtlibArray.get.value)
  // def layout(implicit ctx: Ctx): Doc = ???
}
