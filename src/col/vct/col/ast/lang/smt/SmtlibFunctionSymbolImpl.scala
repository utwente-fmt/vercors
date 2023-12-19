package vct.col.ast.lang.smt

import vct.col.ast.{Applicable, SmtlibFunctionSymbol}
import vct.col.ref.Ref

trait SmtlibFunctionSymbolImpl[G] { this: SmtlibFunctionSymbol[G] =>
  def ref: Ref[G, _ <: Applicable[G]]
}
