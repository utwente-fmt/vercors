package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqReplace
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqReplaceImpl[G] {
  this: Z3SeqReplace[G] =>
  override def t: Type[G] = haystack.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
