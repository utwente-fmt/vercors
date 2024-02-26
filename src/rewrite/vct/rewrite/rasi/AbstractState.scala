package vct.rewrite.rasi

import vct.col.ast._
import vct.col.util.{AstBuildHelpers, Substitute}
import vct.rewrite.cfg.CFGEntry

import scala.collection.immutable.HashMap

case class AbstractState[G](valuations: Map[ConcreteVariable[G], UncertainValue], processes: HashMap[AbstractProcess[G], CFGEntry[G]], lock: Option[AbstractProcess[G]]) {
  def successors(): Set[AbstractState[G]] =
    processes.flatMap(p => p._1.get_next(p._2, this)).toSet

  def with_process_at(process: AbstractProcess[G], position: CFGEntry[G]): AbstractState[G] =
    AbstractState(valuations, processes.removed(process) + (process -> position), lock)

  def without_process(process: AbstractProcess[G]): AbstractState[G] =
    AbstractState(valuations, processes.removed(process), lock)

  def locked_by(process: AbstractProcess[G]): AbstractState[G] = AbstractState(valuations, processes, Some(process))

  def unlocked(): AbstractState[G] = AbstractState(valuations, processes, None)

  def with_valuation(variable: Expr[G], value: UncertainValue): AbstractState[G] = variable_from_expr(variable) match {
    case Some(concrete_variable) => val_updated(concrete_variable, value)
    case None => this
  }
  private def val_updated(variable: ConcreteVariable[G], value: UncertainValue): AbstractState[G] =
    AbstractState(valuations + (variable -> value), processes, lock)

  def with_assumption(assumption: Expr[G]): Set[AbstractState[G]] = resolve_effect(assumption, negate = false)

  private def resolve_effect(assumption: Expr[G], negate: Boolean): Set[AbstractState[G]] = assumption match {
    // Consider boolean/separation logic operators
    case Not(arg) => this.resolve_effect(arg, !negate)
    case Star(left, right) => if (!negate) handle_and(left, right) else handle_or(left, right, neg_left = true, neg_right = true)
    case And(left, right) => if (!negate) handle_and(left, right) else handle_or(left, right, neg_left = true, neg_right = true)
    case Or(left, right) => if (!negate) handle_or(left, right) else handle_and(left, right, neg_left = true, neg_right = true)
    case Implies(left, right) => if (!negate) handle_or(left, right, neg_left = true) else handle_and(left, right, neg_right = true)
    // Atomic comparisons, if they contain a concrete variable, can actually affect the state
    case c: Comparison[G] => handle_update(c, negate)
    // Boolean variables could appear in the assumption without any comparison
    case e: Expr[G] if valuations.keys.exists(v => v.is(e, this)) => Set(this.val_updated(variable_from_expr(e).get, UncertainBooleanValue.from(!negate)))
    case _ => throw new IllegalArgumentException(s"Effect of contract expression ${assumption.toInlineString} is not implemented")
  }

  private def handle_and(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[AbstractState[G]] =
    this.resolve_effect(left, neg_left).flatMap(s => s.resolve_effect(right, neg_right))

  private def handle_or(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[AbstractState[G]] = {
    val pure_left = is_pure(left)
    val pure_right = is_pure(right)
    // If one side is pure and the other has an effect on the state, treat this "or" as an implication. If both sides
    // update the state, treat it as a split in the state space instead
    if (pure_left && pure_right) Set(this)
    else if (pure_left) handle_implies(left, right, !neg_left, neg_right)
    else if (pure_right) handle_implies(right, left, !neg_right, neg_left)
    else this.resolve_effect(left, neg_left) ++ this.resolve_effect(right, neg_right)
  }

  private def handle_implies(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[AbstractState[G]] = {
    val resolve_left: UncertainBooleanValue = resolve_boolean_expression(left)
    var res: Set[AbstractState[G]] = Set()
    if (neg_left && resolve_left.can_be_false || (!neg_left) && resolve_left.can_be_true) res = res ++ resolve_effect(right, neg_right)
    if (neg_left && resolve_left.can_be_true || (!neg_left) && resolve_left.can_be_false) res = res ++ Set(this)
    res
  }

  private def handle_update(comp: Comparison[G], negate: Boolean): Set[AbstractState[G]] = {
    val pure_left = is_pure(comp.left)
    val pure_right = is_pure(comp.right)
    if (pure_left == pure_right) return Set(this)
    // Now, exactly one side is pure, and the other contains a concrete variable
    // Resolve the variable and the other side of the equation    TODO: Reduce the side with the variable if it contains anything else as well
    val variable: ConcreteVariable[G] = if (pure_left) variable_from_expr(comp.right).get else variable_from_expr(comp.left).get
    val value: UncertainValue = if (pure_left) resolve_expression(comp.left) else resolve_expression(comp.right)

    comp match {
      case _: Eq[_] => Set(if (!negate) this.val_updated(variable, value) else this.val_updated(variable, value.complement()))
      case _: Neq[_] => Set(if (!negate) this.val_updated(variable, value.complement()) else this.val_updated(variable, value))
      case AmbiguousGreater(_, _) | Greater(_, _) => bound_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left == negate, negate)
      case AmbiguousGreaterEq(_, _) | GreaterEq(_, _) => bound_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left == negate, !negate)
      case AmbiguousLessEq(_, _) | LessEq(_, _) => bound_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left != negate, !negate)
      case AmbiguousLess(_, _) | Less(_, _) => bound_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left != negate, negate)
    }
  }

  private def bound_variable(v: ConcreteVariable[G], b: UncertainIntegerValue, var_greater: Boolean, can_be_equal: Boolean): Set[AbstractState[G]] = {
    if (var_greater) {
      if (can_be_equal) Set(this.val_updated(v, b.above_eq()))
      else Set(this.val_updated(v, b.above()))
    }
    else {
      if (can_be_equal) Set(this.val_updated(v, b.below_eq()))
      else Set(this.val_updated(v, b.below()))
    }
  }

  private def is_pure(node: Node[G]): Boolean = node match {
    // The old value of a variable is always pure, since it cannot be updated
    case _: Old[_] => true
    case e: UnExpr[_] => e.subnodes.forall(n => is_pure(n))
    case e: BinExpr[_] => e.subnodes.forall(n => is_pure(n))
    case e: Expr[_] => if (valuations.keys.exists(v => v.is(e, this))) false else e.subnodes.forall(n => is_pure(n))
    case _ => true
  }

  def with_postcondition(post: AccountedPredicate[G], args: Map[Variable[G], Expr[G]]): Set[AbstractState[G]] =
    with_assumption(unify_expression(AstBuildHelpers.unfoldPredicate(post).reduce((e1, e2) => Star(e1, e2)(e1.o)), args))

  private def unify_expression(cond: Expr[G], args: Map[Variable[G], Expr[G]]): Expr[G] =
    Substitute(args.map[Expr[G], Expr[G]]{ case (v, e) => Local[G](v.ref)(v.o) -> e }).dispatch(cond)

  def resolve_expression(expr: Expr[G]): UncertainValue = expr.t match {
    case _: IntType[_] => resolve_integer_expression(expr)
    case _: TBool[_] => resolve_boolean_expression(expr)
    case _ => throw new IllegalArgumentException(s"Type ${expr.t.toInlineString} is not supported")
  }

  def resolve_integer_expression(expr: Expr[G]): UncertainIntegerValue = expr match {
    case CIntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case IntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case SizeOf(tname) => UncertainIntegerValue.above(1)    // TODO: Can we use more information about sizeof?
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
    case Old(exp, at) => at match {
      case Some(_) => throw new IllegalArgumentException(s"Cannot resolve labelled old expression ${expr.toInlineString}")
      case None => resolve_integer_expression(exp)
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
    case Old(exp, at) => at match {
      case Some(_) => throw new IllegalArgumentException(s"Cannot resolve labelled old expression ${expr.toInlineString}")
      case None => resolve_boolean_expression(exp)
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
