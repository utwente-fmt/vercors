package vct.rewrite.rasi

import vct.col.ast._

case class AbstractState[G](valuations: Map[ConcreteVariable[G], Int]) {
  def resolve_integer_expression(expr: Expr[G]): Int = expr match {
    case CIntegerValue(value) => value.intValue
    case IntegerValue(value) => value.intValue
    case Local(ref) => ???
    case DerefHeapVariable(ref) => ???
    case Deref(obj, ref) => ???
    case DerefPointer(pointer) => ???
    case SizeOf(tname) => ???
    case UMinus(arg) => -resolve_integer_expression(arg)
    case AmbiguousMult(left, right) => resolve_integer_expression(left) * resolve_integer_expression(right)
    case AmbiguousPlus(left, right) => resolve_integer_expression(left) + resolve_integer_expression(right)
    case AmbiguousMinus(left, right) => resolve_integer_expression(left) - resolve_integer_expression(right)
    case AmbiguousComputationalOr(left, right) => resolve_integer_expression(left) | resolve_integer_expression(right)
    case AmbiguousComputationalXor(left, right) => resolve_integer_expression(left) ^ resolve_integer_expression(right)
    case AmbiguousComputationalAnd(left, right) => resolve_integer_expression(left) & resolve_integer_expression(right)
    case
  }

  def to_expression(): Expr[G] = {
    ???
  }
}
