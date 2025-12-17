package vct.col.ast.expr.heap.alloc

import vct.col.ast.{NewConstPointer, TConstPointer, Type}
import vct.col.print._
import vct.col.ast.ops.NewConstPointerOps

trait NewConstPointerImpl[G] extends NewConstPointerOps[G] {
  this: NewConstPointer[G] =>
  override lazy val t: Type[G] = TConstPointer[G](element)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> "const" <+> element <> "[" <> size <> "]"
}
