package vct.col.ast.declaration.singular

import vct.col.ast.Variable
import vct.col.print._

trait VariableImpl[G] { this: Variable[G] =>
  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.C | Ctx.Cuda | Ctx.OpenCL =>
      val (spec, decl) = t.layoutSplitDeclarator
      spec <+> decl <> ctx.name(this)
    case Ctx.PVL | Ctx.Java => t.show <+> ctx.name(this)
    case Ctx.Silver => Text(ctx.name(this)) <> ":" <+> t
  }
}