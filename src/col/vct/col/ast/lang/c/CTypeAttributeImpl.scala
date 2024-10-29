package vct.col.ast.lang.c

import vct.col.ast.CTypeAttribute
import vct.col.ast.ops.CTypeAttributeOps
import vct.col.print.{Ctx, Doc, Text}

trait CTypeAttributeImpl[G] extends CTypeAttributeOps[G] {
  this: CTypeAttribute[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Text("__attribute__") <> "( (" <> name <> "(" <> Doc.args(args) <> ")" <>
      ") )"
  }
}
