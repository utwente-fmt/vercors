package vct.rewrite.rasi

import vct.col.ast._

case object Utils {
  def resolve_integer_expression[G](expr: Expr[G]): Int = expr match {
    case IntegerValue(value) => value.intValue
    case Plus(left, right) => resolve_integer_expression(left) + resolve_integer_expression(right)

  }
}
