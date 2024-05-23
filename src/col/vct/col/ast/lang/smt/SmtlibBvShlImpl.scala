package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvShl
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvShlOps

trait SmtlibBvShlImpl[G] extends SmtlibBvShlOps[G] { this: SmtlibBvShl[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
