package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibToReal, TRational, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibToRealOps

trait SmtlibToRealImpl[G] extends SmtlibToRealOps[G] { this: SmtlibToReal[G] =>
  override def t: Type[G] = TRational()
  override def layout(implicit ctx: Ctx): Doc = Text("to_real(") <> arg <> ")"
}
