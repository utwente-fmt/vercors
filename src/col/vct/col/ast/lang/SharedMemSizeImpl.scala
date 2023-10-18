package vct.col.ast.lang

import vct.col.ast.{SharedMemSize, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SharedMemSizeImpl[G] {
  this: SharedMemSize[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\shared_mem_size(") <> pointer <> ")"
}
