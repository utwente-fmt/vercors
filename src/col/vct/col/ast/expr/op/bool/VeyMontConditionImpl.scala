package vct.col.ast.expr.op.bool

import vct.col.ast.{TBool, Type, VeyMontCondition}
import vct.col.print._
import vct.col.ast.ops.VeyMontConditionOps

trait VeyMontConditionImpl[G] extends VeyMontConditionOps[G] { this: VeyMontCondition[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.AND
  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.fold(condition.map(_._2).map(assoc))(_ <+> "&&" <+/> _))
}
