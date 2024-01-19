package vct.col.ast.lang.gpgpu

import vct.col.ast.GpgpuBarrier
import vct.col.print.{Ctx, Doc, DocUtil, Show, Text, Group}
import vct.col.ast.ops.GpgpuBarrierOps

trait GpgpuBarrierImpl[G] extends GpgpuBarrierOps[G] { this: GpgpuBarrier[G] =>
  def layoutContract(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      DocUtil.clauses("requires", requires),
      DocUtil.clauses("ensures", ensures),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Doc.spec(Show.lazily(layoutContract(_))),
      Group(
        Text("__vercors_barrier__") <> "(" <> Doc.arg(Doc.fold(specifiers)(_ <+> "|" <+/> _)) <> ")"
      ),
    ))
}