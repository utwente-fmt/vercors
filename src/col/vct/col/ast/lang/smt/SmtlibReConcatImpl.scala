package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReConcat
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReConcatImpl[G] { this: SmtlibReConcat[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
