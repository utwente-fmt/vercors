package vct.col.ast.lang.smt

import vct.col.ast.TSmtlibFloatingPoint
import vct.col.ast.ops.TSmtlibFloatingPointOps
import vct.col.print._

trait TSmtlibFloatingPointImpl[G] extends TSmtlibFloatingPointOps[G] { this: TSmtlibFloatingPoint[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
