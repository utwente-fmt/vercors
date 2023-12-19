package vct.col.ast.lang.gpgpu

import vct.col.ast.{SharedMemSize, TCInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SharedMemSizeOps

trait SharedMemSizeImpl[G] { this: SharedMemSize[G] =>
  override def t: Type[G] = TCInt()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\shared_mem_size(") <> pointer <> ")"
}