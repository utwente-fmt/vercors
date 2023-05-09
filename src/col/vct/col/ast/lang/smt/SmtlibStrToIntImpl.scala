package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrToInt
import vct.col.ast.{Type, TInt}
import vct.col.print._

trait SmtlibStrToIntImpl[G] { this: SmtlibStrToInt[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
