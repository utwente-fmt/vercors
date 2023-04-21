package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqMap
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqMapImpl[G] { this: Z3SeqMap[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
