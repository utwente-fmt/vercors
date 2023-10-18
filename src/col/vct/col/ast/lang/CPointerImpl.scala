package vct.col.ast.lang

import vct.col.ast.CPointer
import vct.col.print.{Ctx, Doc, Text}

trait CPointerImpl[G] {
  this: CPointer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("*") <> Doc.rspread(qualifiers)
}
