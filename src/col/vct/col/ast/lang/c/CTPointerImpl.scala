package vct.col.ast.lang.c

import vct.col.ast.CTPointer
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CTPointerOps

trait CTPointerImpl[G] extends CTPointerOps[G] { this: CTPointer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("pointer") <> open <> innerType <> close
}
