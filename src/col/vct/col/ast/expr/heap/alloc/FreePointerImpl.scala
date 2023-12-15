package vct.col.ast.expr.heap.alloc

import vct.col.ast.{FreePointer, TVoid, Type}
import vct.col.print._

trait FreePointerImpl[G] { this: FreePointer[G] =>
  override lazy val t: Type[G] = TVoid()

  override def layout(implicit ctx: Ctx): Doc =
    Text("free") <+> "(" <> pointer <> ")"
}