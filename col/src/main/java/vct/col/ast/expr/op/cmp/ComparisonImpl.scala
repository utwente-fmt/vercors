package vct.col.ast.expr.op.cmp

import vct.col.ast.{Comparison, TBool, Type}
import vct.col.typerules.Types

trait ComparisonImpl[G] { this: Comparison[G] =>
  override def t: Type[G] = TBool()
  def comparisonType: Type[G] = Types.leastCommonSuperType(left.t, right.t)
}