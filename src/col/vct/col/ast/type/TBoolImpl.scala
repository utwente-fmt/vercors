package vct.col.ast.`type`

import vct.col.ast.TBool
import vct.col.print.{Ctx, Doc, Group, Text}

trait TBoolImpl[G] { this: TBool[G] =>
  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.PVL => Text("boolean")
    case Ctx.Silver => Text("Bool")
    case Ctx.Java => Text("boolean")
    case Ctx.C | Ctx.Cuda | Ctx.OpenCL => Text("bool")
  }
}