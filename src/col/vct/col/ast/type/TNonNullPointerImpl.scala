package vct.col.ast.`type`

import vct.col.ast.TNonNullPointer
import vct.col.ast.ops.TNonNullPointerOps
import vct.col.print._

trait TNonNullPointerImpl[G] extends TNonNullPointerOps[G] {
  this: TNonNullPointer[G] =>
  override def layoutSplitDeclarator(implicit ctx: Ctx): (Doc, Doc) = {
    val (spec, decl) = element.layoutSplitDeclarator
    (spec, decl <> "*")
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("NonNull") <> open <> element <> close)
}
