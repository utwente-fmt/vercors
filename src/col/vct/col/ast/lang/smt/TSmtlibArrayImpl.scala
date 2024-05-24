package vct.col.ast.lang.smt

import vct.col.ast.TSmtlibArray
import vct.col.ast.ops.TSmtlibArrayOps
import vct.col.print._

trait TSmtlibArrayImpl[G] extends TSmtlibArrayOps[G] { this: TSmtlibArray[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
