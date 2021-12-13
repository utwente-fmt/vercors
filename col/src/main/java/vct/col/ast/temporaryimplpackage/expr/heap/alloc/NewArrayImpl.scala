package vct.col.ast.temporaryimplpackage.expr.heap.alloc

import hre.util.FuncTools
import vct.col.ast.{NewArray, TArray, Type}

trait NewArrayImpl[G] { this: NewArray[G] =>
  override def t: Type[G] = FuncTools.repeat[Type[G]](TArray(_), dims.size + moreDims, element)
}