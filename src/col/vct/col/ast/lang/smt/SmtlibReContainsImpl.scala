package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReContains, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReContainsOps

trait SmtlibReContainsImpl[G] extends SmtlibReContainsOps[G] { this: SmtlibReContains[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
