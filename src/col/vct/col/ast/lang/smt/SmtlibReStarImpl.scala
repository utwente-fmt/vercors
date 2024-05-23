package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReStar, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReStarOps

trait SmtlibReStarImpl[G] extends SmtlibReStarOps[G] { this: SmtlibReStar[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
