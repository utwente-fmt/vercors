package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStore
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibStoreOps

trait SmtlibStoreImpl[G] extends SmtlibStoreOps[G] { this: SmtlibStore[G] =>
  override def t: Type[G] = arr.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
