package vct.col.ast.lang.smt

import vct.col.ast.TSmtlibSeq
import vct.col.ast.ops.TSmtlibSeqOps
import vct.col.print._

trait TSmtlibSeqImpl[G] extends TSmtlibSeqOps[G] { this: TSmtlibSeq[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
