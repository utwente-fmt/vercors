package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqMapI
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqMapIImpl[G] { this: Z3SeqMapI[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
