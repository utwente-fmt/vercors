package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTStruct
import vct.col.ast.ops.LLVMTStructOps
import vct.col.print._
import vct.col.typerules.TypeSize

trait LLVMTStructImpl[G] extends LLVMTStructOps[G] {
  this: LLVMTStruct[G] =>

  // Ensure that the struct elements are sorted and there are no overlapping elements
  assert(elements.sliding(2).forall {
    case Seq(a, b) => a.offset <= b.offset && a.offset + a.size <= b.offset
    case Seq(_) => true
  })

  private def layoutPacked(inner: Doc)(implicit ctx: Ctx): Doc =
    if (packed) { Text("<") <> inner <> ">" }
    else { inner }

  override def layout(implicit ctx: Ctx): Doc = {
    if (name.nonEmpty)
      Text(name.head)
    else
      (layoutPacked(Text("{") <> Doc.args(elements) <> "}"))
  }

  override def bits: TypeSize = { TypeSize.Exact(sizeInBits) }
}
