package vct.col.ast.`type`

import vct.col.ast.TAxiomatic
import vct.col.print.{Ctx, Doc, Text, Empty, Group}
import vct.col.ast.ops.TAxiomaticOps

trait TAxiomaticImpl[G] extends TAxiomaticOps[G] {
  this: TAxiomatic[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Group(
      ctx.syntax match {
        case Ctx.Isar =>
          (if (args.isEmpty)
            Empty
          else
            open <> Doc.args(args) <> close
            ) <+> Text(ctx.name(adt))
        case _ =>
          Text(ctx.name(adt)) <>
          (if (args.isEmpty)
            Empty
          else
            open <> Doc.args(args) <> close)
      }
    )
  }
}
