package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3SeqSuffixOf}
import vct.col.print._

trait Z3SeqSuffixOfImpl[G] { this: Z3SeqSuffixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
