package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqExtract
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqExtractImpl[G] { this: Z3SeqExtract[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
