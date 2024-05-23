package vct.col.ast.expr.resource

import vct.col.ast.{PolarityDependent, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.typerules.Types
import vct.col.ast.ops.PolarityDependentOps

trait PolarityDependentImpl[G] extends PolarityDependentOps[G] {
  this: PolarityDependent[G] =>
  override lazy val t: Type[G] = Types
    .leastCommonSuperType(onInhale.t, onExhale.t)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("\\polarity_dependent(") <> Doc.args(Seq(onInhale, onExhale)) <> ")"
    )
}
