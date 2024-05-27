package vct.col.ast.lang.smt

import vct.col.ast.{TBool, Type, Z3SeqSuffixOf}
import vct.col.print._
import vct.col.ast.ops.Z3SeqSuffixOfOps

trait Z3SeqSuffixOfImpl[G] extends Z3SeqSuffixOfOps[G] {
  this: Z3SeqSuffixOf[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
