package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReUnion, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReUnionOps

trait SmtlibReUnionImpl[G] extends SmtlibReUnionOps[G] { this: SmtlibReUnion[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
