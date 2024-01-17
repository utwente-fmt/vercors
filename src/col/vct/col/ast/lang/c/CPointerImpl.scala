package vct.col.ast.lang.c

import vct.col.ast.CPointer
import vct.col.ast.ops.{CPointerFamilyOps, CPointerOps}
import vct.col.print.{Ctx, Doc, Text}

trait CPointerImpl[G] extends CPointerOps[G] with CPointerFamilyOps[G] { this: CPointer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("*") <> Doc.rspread(qualifiers)
}