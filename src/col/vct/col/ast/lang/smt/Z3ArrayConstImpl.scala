package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibArray, Type, Z3ArrayConst}
import vct.col.print._
import vct.col.ast.ops.Z3ArrayConstOps

trait Z3ArrayConstImpl[G] extends Z3ArrayConstOps[G] {
  this: Z3ArrayConst[G] =>
  override def t: Type[G] = TSmtlibArray(domain, codomain)
  // def layout(implicit ctx: Ctx): Doc = ???
}
