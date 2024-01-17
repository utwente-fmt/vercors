package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsNormal, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpIsNormalOps

trait SmtlibFpIsNormalImpl[G] extends SmtlibFpIsNormalOps[G] { this: SmtlibFpIsNormal[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
