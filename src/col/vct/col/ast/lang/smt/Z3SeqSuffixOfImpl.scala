package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqSuffixOf
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqSuffixOfImpl[G] { this: Z3SeqSuffixOf[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
