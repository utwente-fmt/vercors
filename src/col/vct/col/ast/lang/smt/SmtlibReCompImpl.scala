package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReComp, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReCompOps

trait SmtlibReCompImpl[G] extends SmtlibReCompOps[G] { this: SmtlibReComp[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
