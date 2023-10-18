package vct.col.ast.lang

import vct.col.ast.{GpgpuAtomic, Block}
import vct.col.print._

trait GpgpuAtomicImpl[G] {
  this: GpgpuAtomic[G] =>
  def layoutWith(implicit ctx: Ctx): Doc =
    Group(Text("with") <+> before.layoutAsBlock)

  def layoutThen(implicit ctx: Ctx): Doc =
    Group(Text("then") <+> after.layoutAsBlock)

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      if (impl == Block[G](Nil))
        Empty
      else
        Doc.inlineSpec(Show.lazily(layoutWith(_))),
      Text("__vercors_atomic__") <+> impl.layoutAsBlock,
      if (impl == Block[G](Nil))
        Empty
      else
        Doc.inlineSpec(Show.lazily(layoutThen(_))),
    ))
}
