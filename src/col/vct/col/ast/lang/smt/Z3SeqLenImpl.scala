package vct.col.ast.lang.smt

import vct.col.ast.{TInt, Type, Z3SeqLen}
import vct.col.print._

trait Z3SeqLenImpl[G] {
  this: Z3SeqLen[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
