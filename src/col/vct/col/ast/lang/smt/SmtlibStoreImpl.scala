package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStore
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStoreImpl[G] { this: SmtlibStore[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
