package vct.col.ast.lang.smt

import vct.col.ast.{Applicable, SmtlibFunctionSymbol}
import vct.col.ref.Ref
import vct.col.ast.ops.SmtlibFunctionSymbolFamilyOps

trait SmtlibFunctionSymbolImpl[G] extends SmtlibFunctionSymbolFamilyOps[G] { this: SmtlibFunctionSymbol[G] =>
  def ref: Ref[G, _ <: Applicable[G]]
}
