package vct.col.ast.lang.smt

import vct.col.ast.{TSmtlibArray, Type, Z3ArrayConst}
import vct.col.print._

trait Z3ArrayConstImpl[G] { this: Z3ArrayConst[G] =>
  override def t: Type[G] = TSmtlibArray(domain, codomain)
  // def layout(implicit ctx: Ctx): Doc = ???
}
