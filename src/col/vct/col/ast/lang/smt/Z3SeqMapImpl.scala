package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibSeq, Type, Z3SeqMap}
import vct.col.print._
import vct.col.ast.ops.Z3SeqMapOps

trait Z3SeqMapImpl[G] extends Z3SeqMapOps[G] { this: Z3SeqMap[G] =>
  override def t: Type[G] = TSmtlibSeq(f.t.asSmtlibArray.get.value)
  // def layout(implicit ctx: Ctx): Doc = ???
}
