package vct.col.ast.`type`

import vct.col.ast.TAxiomatic
import vct.col.print.{Ctx, Doc, Text, Empty, Group}
import vct.col.ast.ops.TAxiomaticOps

trait TAxiomaticImpl[G] extends TAxiomaticOps[G] {
  this: TAxiomatic[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(ctx.name(adt)) <>
        (if (args.isEmpty)
           Empty
         else
           open <> Doc.args(args) <> close)
    )
}
