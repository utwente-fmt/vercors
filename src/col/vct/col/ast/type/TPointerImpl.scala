package vct.col.ast.`type`

import vct.col.ast.TPointer
import vct.col.print._
import vct.col.ast.ops.TPointerOps

trait TPointerImpl[G] extends TPointerOps[G] { this: TPointer[G] =>
  override def layoutSplitDeclarator(implicit ctx: Ctx): (Doc, Doc) = {
    val (spec, decl) = element.layoutSplitDeclarator
    (spec, decl <> "*")
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("pointer") <> open <> element <> close)
}