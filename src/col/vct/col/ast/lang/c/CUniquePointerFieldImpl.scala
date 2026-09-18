package vct.col.ast.lang.c

import vct.col.ast.CUniquePointerField
import vct.col.ast.ops.CUniquePointerFieldOps
import vct.col.print._

trait CUniquePointerFieldImpl[G] extends CUniquePointerFieldOps[G] { this: CUniquePointerField[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("unique_pointer_field<") <> name <>","<> i.toString() <> ">")
}
