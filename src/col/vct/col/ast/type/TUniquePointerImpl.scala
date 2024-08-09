package vct.col.ast.`type`

import vct.col.ast.TUniquePointer
import vct.col.ast.ops.TUniquePointerOps
import vct.col.print._

trait TUniquePointerImpl[G] extends TUniquePointerOps[G] {
  this: TUniquePointer[G] =>
  override def layoutSplitDeclarator(implicit ctx: Ctx): (Doc, Doc) = {
    val (spec, decl) = element.layoutSplitDeclarator
    (spec, decl <> "*")
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("unique_pointer") <> open <> element <> "," <> id.toString <> close)
}
