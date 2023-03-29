package vct.col.ast.expr.op.bool

import vct.col.ast.{TBool, Type, VeyMontCondition}

trait VeyMontConditionImpl[G] { this: VeyMontCondition[G] =>
  override def t: Type[G] = TBool()
}
