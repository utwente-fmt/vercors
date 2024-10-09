package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTStruct
import vct.col.ast.ops.LLVMTStructOps
import vct.col.print._

trait LLVMTStructImpl[G] extends LLVMTStructOps[G] {
  this: LLVMTStruct[G] =>

  private def layoutPacked(inner: Doc)(implicit ctx: Ctx): Doc =
    if (packed) { Text("<") <> inner <> ">" }
    else { inner }

  override def layout(implicit ctx: Ctx): Doc = {
    if (name.isDefined)
      Text(name.get)
    else
      (layoutPacked(Text("{") <> Doc.args(elements) <> "}"))
  }
}
