package vct.col.ast.lang.smt

import vct.col.ast.TSmtlibRegLan
import vct.col.ast.ops.TSmtlibRegLanOps
import vct.col.print._

trait TSmtlibRegLanImpl[G] extends TSmtlibRegLanOps[G] { this: TSmtlibRegLan[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
