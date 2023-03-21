package vct.col.ast.expr.heap.alloc

import hre.util.FuncTools
import vct.col.ast.{NewArray, TArray, Type}

trait NewArrayImpl[G] { this: NewArray[G] =>
  override lazy val t: Type[G] = FuncTools.repeat[Type[G]](TArray(_), dims.size + moreDims, element)
}