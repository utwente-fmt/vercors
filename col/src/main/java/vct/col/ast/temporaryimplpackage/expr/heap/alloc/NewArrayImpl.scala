package vct.col.ast.temporaryimplpackage.expr.heap.alloc

import hre.util.FuncTools
import vct.col.ast.{NewArray, TArray, Type}

trait NewArrayImpl { this: NewArray =>
  override def t: Type = FuncTools.repeat(TArray(_), dims.size + moreDims, element)
}