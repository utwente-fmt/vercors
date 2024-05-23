package vct.col.ast.`type`

import vct.col.ast.TBool
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TBoolOps

trait TBoolImpl[G] extends TBoolOps[G] {
  this: TBool[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.PVL => Text("boolean")
      case Ctx.Silver => Text("Bool")
      case Ctx.Java => Text("boolean")
      case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP => Text("bool")
    }
}
