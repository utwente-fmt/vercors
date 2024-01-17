package vct.col.ast.lang.smt

import vct.col.ast.TSmtlibRoundingMode
import vct.col.ast.ops.TSmtlibRoundingModeOps
import vct.col.print._

trait TSmtlibRoundingModeImpl[G] extends TSmtlibRoundingModeOps[G] { this: TSmtlibRoundingMode[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
