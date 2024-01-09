package vct.col.ast.lang.smt

import vct.col.ast.TSmtlibString
import vct.col.ast.ops.TSmtlibStringOps
import vct.col.print._

trait TSmtlibStringImpl[G] extends TSmtlibStringOps[G] { this: TSmtlibString[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
