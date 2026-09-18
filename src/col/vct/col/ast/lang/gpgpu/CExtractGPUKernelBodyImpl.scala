package vct.col.ast.lang.gpgpu

import vct.col.ast.CExtractGPUKernelBody
import vct.col.ast.ops.CExtractGPUKernelBodyOps
import vct.col.print.{Ctx, Doc, Text}

trait CExtractGPUKernelBodyImpl[G] extends CExtractGPUKernelBodyOps[G] {
  this: CExtractGPUKernelBody[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Text("extract_body"))
}
