package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3SeqPrefixOf}
import vct.col.print._
import vct.col.ast.ops.Z3SeqPrefixOfOps

trait Z3SeqPrefixOfImpl[G] extends Z3SeqPrefixOfOps[G] { this: Z3SeqPrefixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
