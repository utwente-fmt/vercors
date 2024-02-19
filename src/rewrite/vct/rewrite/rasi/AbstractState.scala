package vct.rewrite.rasi

import vct.col.ast._
import vct.col.origin.{LabelContext, Origin}
import vct.rewrite.cfg.CFGEntry

import scala.collection.immutable.HashMap

case class AbstractState[G](valuations: Map[ConcreteVariable[G], Int], processes: HashMap[AbstractProcess[G], CFGEntry[G]]) {
  def successors(): Set[AbstractState[G]] =
    processes.flatMap(p => p._1.get_next(p._2, this)).toSet

  def resolve_integer_expression(expr: Expr[G]): UncertainIntegerValue = expr match {
    case CIntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case IntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case Local(ref) => ???
    case DerefHeapVariable(ref) => ???
    case Deref(obj, ref) => ???
    case DerefPointer(pointer) => ???
    case SizeOf(tname) => ???
    case UMinus(arg) => -resolve_integer_expression(arg)
    case AmbiguousMult(left, right) => resolve_integer_expression(left) * resolve_integer_expression(right)
    case AmbiguousPlus(left, right) => resolve_integer_expression(left) + resolve_integer_expression(right)
    case AmbiguousMinus(left, right) => resolve_integer_expression(left) - resolve_integer_expression(right)
    case Exp(left, right) => resolve_integer_expression(left).pow(resolve_integer_expression(right))
    case Plus(left, right) => resolve_integer_expression(left) + resolve_integer_expression(right)
    case Minus(left, right) => resolve_integer_expression(left) - resolve_integer_expression(right)
    case Mult(left, right) => resolve_integer_expression(left) * resolve_integer_expression(right)
    case FloorDiv(left, right) => resolve_integer_expression(left) / resolve_integer_expression(right)
    case Mod(left, right) => resolve_integer_expression(left) % resolve_integer_expression(right)
    // TODO: Support bit operations
    case BitNot(arg) => ??? // ~resolve_integer_expression(arg)
    case AmbiguousComputationalOr(left, right) => ??? // resolve_integer_expression(left) | resolve_integer_expression(right)
    case AmbiguousComputationalXor(left, right) => ??? // resolve_integer_expression(left) ^ resolve_integer_expression(right)
    case AmbiguousComputationalAnd(left, right) => ??? // resolve_integer_expression(left) & resolve_integer_expression(right)
    case ComputationalOr(left, right) => ??? // resolve_integer_expression(left) | resolve_integer_expression(right)
    case ComputationalXor(left, right) => ??? // resolve_integer_expression(left) ^ resolve_integer_expression(right)
    case ComputationalAnd(left, right) => ??? // resolve_integer_expression(left) & resolve_integer_expression(right)
    case BitAnd(left, right) => ??? // resolve_integer_expression(left) & resolve_integer_expression(right)
    case BitOr(left, right) => ??? // resolve_integer_expression(left) | resolve_integer_expression(right)
    case BitXor(left, right) => ??? // resolve_integer_expression(left) ^ resolve_integer_expression(right)
    case BitShl(left, right) => ??? // resolve_integer_expression(left) << resolve_integer_expression(right)
    case BitShr(left, right) => ??? // resolve_integer_expression(left) >> resolve_integer_expression(right)
    case BitUShr(left, right) => ??? // resolve_integer_expression(left) >>> resolve_integer_expression(right)
    case Select(cond, ift, iff) => {
      var value: UncertainIntegerValue = UncertainIntegerValue.empty()
      if (resolve_boolean_expression(cond).can_be_true) value = value.union(resolve_integer_expression(ift))
      if (resolve_boolean_expression(cond).can_be_false) value = value.union(resolve_integer_expression(iff))
      value
    }
    case AmbiguousSubscript(collection, index) => ???
    case SeqSubscript(seq, index) => ???
    case ArraySubscript(arr, index) => ???
    case PointerSubscript(pointer, index) => ???
    case Length(arr) => ???
    case Size(obj) => ???
  }

  def resolve_boolean_expression(expr: Expr[G]): UncertainBooleanValue = expr match {
    case BooleanValue(value) => UncertainBooleanValue(value, !value)
    case Local(ref) => ???
    case DerefHeapVariable(ref) => ???
    case Deref(obj, ref) => ???
    case DerefPointer(pointer) => ???
    case Not(arg) => !resolve_boolean_expression(arg)
    case AmbiguousOr(left, right) => resolve_boolean_expression(left) || resolve_boolean_expression(right)
    case And(left, right) => resolve_boolean_expression(left) && resolve_boolean_expression(right)
    case Or(left, right) => resolve_boolean_expression(left) || resolve_boolean_expression(right)
    case Implies(left, right) => (!resolve_boolean_expression(left)) || resolve_boolean_expression(right)
    case Eq(left, right) => resolve_integer_expression(left) == resolve_integer_expression(right)
    case Neq(left, right) => resolve_integer_expression(left) != resolve_integer_expression(right)
    case AmbiguousGreater(left, right) => resolve_integer_expression(left) > resolve_integer_expression(right)
    case AmbiguousLess(left, right) => resolve_integer_expression(left) < resolve_integer_expression(right)
    case AmbiguousGreaterEq(left, right) => resolve_integer_expression(left) >= resolve_integer_expression(right)
    case AmbiguousLessEq(left, right) => resolve_integer_expression(left) <= resolve_integer_expression(right)
    case Greater(left, right) => resolve_integer_expression(left) > resolve_integer_expression(right)
    case Less(left, right) => resolve_integer_expression(left) < resolve_integer_expression(right)
    case GreaterEq(left, right) => resolve_integer_expression(left) >= resolve_integer_expression(right)
    case LessEq(left, right) => resolve_integer_expression(left) <= resolve_integer_expression(right)
    case c: SetComparison[G] => ???
    case Select(cond, ift, iff) => {
      var value: UncertainBooleanValue = UncertainBooleanValue(can_be_true = false, can_be_false = false)
      if (resolve_boolean_expression(cond).can_be_true) value = value.union(resolve_boolean_expression(ift))
      if (resolve_boolean_expression(cond).can_be_false) value = value.union(resolve_boolean_expression(iff))
      value
    }
    case AmbiguousSubscript(collection, index) => ???
    case SeqSubscript(seq, index) => ???
    case ArraySubscript(arr, index) => ???
    case PointerSubscript(pointer, index) => ???
  }

  private def neutral_element: Expr[G] = BooleanValue(value = true)(Origin(Seq(LabelContext("neutral element for and"))))
  def to_expression(): Expr[G] = {
    valuations.map(v => Eq(v._1.to_expression(), IntegerValue(v._2)(Origin(Seq(LabelContext("int value")))))(Origin(Seq(LabelContext("valuation")))))
              .fold(neutral_element)((e1, e2) => And(e1, e2)(e1.o))
  }
}
