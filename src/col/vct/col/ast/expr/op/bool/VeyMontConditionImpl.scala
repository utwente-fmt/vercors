package vct.col.ast.expr.op.bool

import vct.col.ast.{TBool, Type, VeyMontCondition}
import vct.col.print._

trait VeyMontConditionImpl[G] { this: VeyMontCondition[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.AND
  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.fold(condition.map(_._2).map(assoc))(_ <+> "&&" <+/> _))
}
