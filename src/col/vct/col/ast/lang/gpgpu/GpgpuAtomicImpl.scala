package vct.col.ast.lang.gpgpu

import vct.col.ast.{GpgpuAtomic, Block}
import vct.col.print._
import vct.col.ast.ops.GpgpuAtomicOps

trait GpgpuAtomicImpl[G] extends GpgpuAtomicOps[G] { this: GpgpuAtomic[G] =>
  def layoutWith(implicit ctx: Ctx): Doc = Group(Text("with") <+> before.layoutAsBlock)

  def layoutThen(implicit ctx: Ctx): Doc = Group(Text("then") <+> after.layoutAsBlock)

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      if(impl == Block[G](Nil)) Empty else Doc.inlineSpec(Show.lazily(layoutWith(_))),
      Text("__vercors_atomic__") <+> impl.layoutAsBlock,
      if(impl == Block[G](Nil)) Empty else Doc.inlineSpec(Show.lazily(layoutThen(_))),
    ))
}