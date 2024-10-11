package vct.col.ast.declaration.cls

import vct.col.ast.{Final, InstanceField}
import vct.col.print._
import vct.col.ast.ops.InstanceFieldOps

trait InstanceFieldImpl[G] extends InstanceFieldOps[G] {
  this: InstanceField[G] =>
  def isFinal = flags.collectFirst { case _: Final[G] => () }.isDefined

  override def layout(implicit ctx: Ctx): Doc =
    Doc.rspread(flags) <>
      (ctx.syntax match {
        case Ctx.C | Ctx.Cuda | Ctx.OpenCL | Ctx.CPP =>
          val (spec, decl) = t.layoutSplitDeclarator
          spec <+> decl <> ctx.name(this) <> ";"
        case Ctx.PVL | Ctx.Java => t.show <+> ctx.name(this) <> ";"
        case Ctx.Silver => Text(ctx.name(this)) <> ":" <+> t
      })
}
