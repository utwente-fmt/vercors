package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReStar
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReStarImpl[G] { this: SmtlibReStar[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
