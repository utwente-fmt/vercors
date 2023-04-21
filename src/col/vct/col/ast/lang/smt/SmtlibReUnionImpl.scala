package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReUnion
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReUnionImpl[G] { this: SmtlibReUnion[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
