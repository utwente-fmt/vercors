package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqPrefixOf
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqPrefixOfImpl[G] { this: Z3SeqPrefixOf[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
