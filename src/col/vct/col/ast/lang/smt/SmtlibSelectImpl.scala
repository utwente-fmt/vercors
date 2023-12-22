package vct.col.ast.lang.smt

import vct.col.ast.SmtlibSelect
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibSelectOps

trait SmtlibSelectImpl[G] extends SmtlibSelectOps[G] { this: SmtlibSelect[G] =>
  override def t: Type[G] = arr.t.asSmtlibArray.get.value
  // def layout(implicit ctx: Ctx): Doc = ???
}
