package vct.col.ast.lang.smt

import vct.col.ast.SmtlibProverFunctionSymbol
import vct.col.ast.ops.SmtlibProverFunctionSymbolOps
import vct.col.print._

trait SmtlibProverFunctionSymbolImpl[G] extends SmtlibProverFunctionSymbolOps[G] { this: SmtlibProverFunctionSymbol[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
