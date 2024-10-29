package vct.col.ast.declaration.global

import vct.col.ast.HeapVariable
import vct.col.print._
import vct.col.ast.ops.HeapVariableOps

trait HeapVariableImpl[G] extends HeapVariableOps[G] {
  this: HeapVariable[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP =>
        val (spec, decl) = t.layoutSplitDeclarator
        spec <+> decl <> ctx.name(this) <> ";"
      case _ => t.show <+> ctx.name(this) <> ";"
    }
}
