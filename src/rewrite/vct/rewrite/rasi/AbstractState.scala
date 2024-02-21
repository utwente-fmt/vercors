package vct.rewrite.rasi

import vct.col.ast._
import vct.rewrite.cfg.CFGEntry

import scala.collection.immutable.HashMap

case class AbstractState[G](valuations: Map[ConcreteVariable[G], UncertainValue], processes: HashMap[AbstractProcess[G], CFGEntry[G]], lock: Option[AbstractProcess[G]]) {
  def successors(): Set[AbstractState[G]] =
    processes.flatMap(p => p._1.get_next(p._2, this)).toSet

  def with_process_at(process: AbstractProcess[G], position: CFGEntry[G]): AbstractState[G] =
    AbstractState(valuations, processes + (process -> position), lock)

  def without_process(process: AbstractProcess[G]): AbstractState[G] =
    AbstractState(valuations, processes.removed(process), lock)

  def with_valuation(variable: Expr[G], value: UncertainValue): AbstractState[G] = variable_from_expr(variable) match {
    case Some(concrete_variable) => AbstractState(valuations + (concrete_variable -> value), processes, lock)
    case None => this
  }

  def locked_by(process: AbstractProcess[G]): AbstractState[G] = AbstractState(valuations, processes, Some(process))

  def unlocked(): AbstractState[G] = AbstractState(valuations, processes, None)

  def resolve_expression(expr: Expr[G]): UncertainValue = expr.t match {
    case _: IntType[_] => resolve_integer_expression(expr)
    case _: TBool[_] => resolve_boolean_expression(expr)
    case _ => throw new IllegalArgumentException(s"Type ${expr.t.toInlineString} is not supported")
  }

  def resolve_integer_expression(expr: Expr[G]): UncertainIntegerValue = expr match {
    case CIntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case IntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case SizeOf(tname) => UncertainIntegerValue.above(0)    // TODO: Can we use more information about sizeof?
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
    // Bit operations destroy any knowledge of integer state       TODO: Support bit operations
    case BitNot(_) => UncertainIntegerValue.uncertain()
    case AmbiguousComputationalOr(_, _) => UncertainIntegerValue.uncertain()
    case AmbiguousComputationalXor(_, _) => UncertainIntegerValue.uncertain()
    case AmbiguousComputationalAnd(_, _) => UncertainIntegerValue.uncertain()
    case ComputationalOr(_, _) => UncertainIntegerValue.uncertain()
    case ComputationalXor(_, _) => UncertainIntegerValue.uncertain()
    case ComputationalAnd(_, _) => UncertainIntegerValue.uncertain()
    case BitAnd(_, _) => UncertainIntegerValue.uncertain()
    case BitOr(_, _) => UncertainIntegerValue.uncertain()
    case BitXor(_, _) => UncertainIntegerValue.uncertain()
    case BitShl(_, _) => UncertainIntegerValue.uncertain()
    case BitShr(_, _) => UncertainIntegerValue.uncertain()
    case BitUShr(_, _) => UncertainIntegerValue.uncertain()
    case Select(cond, ift, iff) => {
      var value: UncertainIntegerValue = UncertainIntegerValue.empty()
      if (resolve_boolean_expression(cond).can_be_true) value = value.union(resolve_integer_expression(ift))
      if (resolve_boolean_expression(cond).can_be_false) value = value.union(resolve_integer_expression(iff))
      value
    }
    case Local(_) | DerefHeapVariable(_) | Deref(_, _) | DerefPointer(_) | AmbiguousSubscript(_, _) | SeqSubscript(_, _) | ArraySubscript(_, _) | PointerSubscript(_, _) => variable_from_expr(expr) match {
      case Some(v) => valuations(v).asInstanceOf[UncertainIntegerValue]
      case None => UncertainIntegerValue.uncertain()
    }
    case Length(arr) => UncertainIntegerValue.above(0)    // TODO: Use contextual information from the global invariant
    case Size(obj) => UncertainIntegerValue.above(0)      //  here as well
    case ProcedureInvocation(_, _, _, _, _, _) => UncertainIntegerValue.uncertain()   // TODO: return value from procedure/method?
    case MethodInvocation(_, _, _, _, _, _, _) => UncertainIntegerValue.uncertain()
  }

  def resolve_boolean_expression(expr: Expr[G]): UncertainBooleanValue = expr match {
    case BooleanValue(value) => UncertainBooleanValue.from(value)
    case Not(arg) => !resolve_boolean_expression(arg)
    case AmbiguousOr(left, right) => resolve_boolean_expression(left) || resolve_boolean_expression(right)
    case And(left, right) => resolve_boolean_expression(left) && resolve_boolean_expression(right)
    case Or(left, right) => resolve_boolean_expression(left) || resolve_boolean_expression(right)
    case Implies(left, right) => (!resolve_boolean_expression(left)) || resolve_boolean_expression(right)
    case Eq(left, right) => resolve_expression(left) == resolve_expression(right)
    case Neq(left, right) => resolve_expression(left) != resolve_expression(right)
    case AmbiguousGreater(left, right) => resolve_integer_expression(left) > resolve_integer_expression(right)
    case AmbiguousLess(left, right) => resolve_integer_expression(left) < resolve_integer_expression(right)
    case AmbiguousGreaterEq(left, right) => resolve_integer_expression(left) >= resolve_integer_expression(right)
    case AmbiguousLessEq(left, right) => resolve_integer_expression(left) <= resolve_integer_expression(right)
    case Greater(left, right) => resolve_integer_expression(left) > resolve_integer_expression(right)
    case Less(left, right) => resolve_integer_expression(left) < resolve_integer_expression(right)
    case GreaterEq(left, right) => resolve_integer_expression(left) >= resolve_integer_expression(right)
    case LessEq(left, right) => resolve_integer_expression(left) <= resolve_integer_expression(right)
    case c: SetComparison[G] => UncertainBooleanValue.uncertain()   // TODO: Implement?
    case Select(cond, ift, iff) => {
      var value: UncertainBooleanValue = UncertainBooleanValue(can_be_true = false, can_be_false = false)
      if (resolve_boolean_expression(cond).can_be_true) value = value.union(resolve_boolean_expression(ift))
      if (resolve_boolean_expression(cond).can_be_false) value = value.union(resolve_boolean_expression(iff))
      value
    }
    case Local(_) | DerefHeapVariable(_) | Deref(_, _) | DerefPointer(_) | AmbiguousSubscript(_, _) | SeqSubscript(_, _) | ArraySubscript(_, _) | PointerSubscript(_, _) => variable_from_expr(expr) match {
      case Some(v) => valuations(v).asInstanceOf[UncertainBooleanValue]
      case None => UncertainBooleanValue.uncertain()
    }
    case ProcedureInvocation(_, _, _, _, _, _) => UncertainBooleanValue.uncertain()   // TODO: return value from procedure/method?
    case MethodInvocation(_, _, _, _, _, _, _) => UncertainBooleanValue.uncertain()
  }

  private def variable_from_expr(variable: Expr[G]): Option[ConcreteVariable[G]] = {
    valuations.keys.collectFirst{ case c: ConcreteVariable[G] if c.is(variable, this) => c }
  }

  def to_expression: Expr[G] = valuations.map(v => v._2.to_expression(v._1.to_expression)).reduce((e1, e2) => And(e1, e2)(e1.o))
}
