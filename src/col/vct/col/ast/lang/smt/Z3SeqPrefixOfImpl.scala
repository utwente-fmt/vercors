package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3SeqPrefixOf}
import vct.col.print._

trait Z3SeqPrefixOfImpl[G] {
  this: Z3SeqPrefixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
