package vct.col.ast.lang.smt

import vct.col.ast.{TInt, Type, Z3SeqLen}
import vct.col.print._
import vct.col.ast.ops.Z3SeqLenOps

trait Z3SeqLenImpl[G] extends Z3SeqLenOps[G] { this: Z3SeqLen[G] =>
  override def t: Type[G] = TInt()
  // def layout(implicit ctx: Ctx): Doc = ???
}
