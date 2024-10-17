package vct.col.ast.declaration.singular

import vct.col.ast.LocalHeapVariable
import vct.col.print._
import vct.col.ast.ops.{LocalHeapVariableOps, VariableFamilyOps}
import vct.col.ast.ops.{LocalHeapVariableOps, LocalHeapVariableFamilyOps}

trait LocalHeapVariableImpl[G]
    extends LocalHeapVariableOps[G] with LocalHeapVariableFamilyOps[G] {
  this: LocalHeapVariable[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("@heap") <+>
      (ctx.syntax match {
        case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP =>
          val (spec, decl) = t.layoutSplitDeclarator
          spec <+> decl <> ctx.name(this)
        case Ctx.PVL | Ctx.Java => t.show <+> ctx.name(this)
        case Ctx.Silver => Text(ctx.name(this)) <> ":" <+> t
      })
}
