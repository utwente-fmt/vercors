package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReFromStr
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReFromStrImpl[G] { this: SmtlibReFromStr[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
