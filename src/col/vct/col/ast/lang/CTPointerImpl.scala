package vct.col.ast.lang

import vct.col.ast.CTPointer
import vct.col.print.{Ctx, Doc, Text}

trait CTPointerImpl[G] {
  this: CTPointer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("pointer") <> open <> innerType <> close
}
