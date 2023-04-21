package vct.col.ast.lang.smt

import vct.col.ast.Z3SeqLen
import vct.col.ast.Type
import vct.col.print._

trait Z3SeqLenImpl[G] { this: Z3SeqLen[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
