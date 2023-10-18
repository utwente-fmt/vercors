package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrIndexOf, TInt, Type}
import vct.col.print._

trait SmtlibStrIndexOfImpl[G] {
  this: SmtlibStrIndexOf[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
