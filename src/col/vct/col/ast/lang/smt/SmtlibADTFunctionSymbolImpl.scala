package vct.col.ast.lang.smt

import vct.col.ast.SmtlibADTFunctionSymbol
import vct.col.ast.ops.SmtlibADTFunctionSymbolOps
import vct.col.print._

trait SmtlibADTFunctionSymbolImpl[G] extends SmtlibADTFunctionSymbolOps[G] { this: SmtlibADTFunctionSymbol[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
