package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewConstPointerArray, TConstPointer, Type}
import vct.col.ast.ops.NewConstPointerArrayOps
import vct.col.print._

trait NewConstPointerArrayImpl[G] extends NewConstPointerArrayOps[G] { this: NewConstPointerArray[G] =>
  override lazy val t: Type[G] = TConstPointer[G](element)
}
