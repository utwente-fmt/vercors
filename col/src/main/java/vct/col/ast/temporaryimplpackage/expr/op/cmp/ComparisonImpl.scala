package vct.col.ast.temporaryimplpackage.expr.op.cmp

import vct.col.ast.{Comparison, TBool, Type}
import vct.col.util.Types

trait ComparisonImpl { this: Comparison =>
  override def t: Type = TBool()
  def comparisonType: Type = Types.leastCommonSuperType(left.t, right.t)
}