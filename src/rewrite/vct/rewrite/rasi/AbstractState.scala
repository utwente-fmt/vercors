package vct.rewrite.rasi

import vct.col.ast._
import vct.col.util.{AstBuildHelpers, Substitute}
import vct.rewrite.cfg.CFGEntry

import scala.collection.immutable.HashMap

case class AbstractState[G](valuations: Map[ConcreteVariable[G], UncertainValue],
                            processes: HashMap[AbstractProcess[G], CFGEntry[G]],
                            lock: Option[AbstractProcess[G]],
                            seq_lengths: Map[InstanceField[G], UncertainIntegerValue]) {
  /**
   * Main function of the abstract state. For all processes that could potentially run, execute all possible next steps.
   *
   * @return The set of possible successor states
   */
  def successors(): Set[AbstractState[G]] =
    processes.flatMap(p => p._1.get_next(p._2, this)).toSet

  /**
   * Updates the state by changing the program counter for a process.
   *
   * @param process Process to update
   * @param position New position of the process
   * @return An abstract state that is a copy of this one with the updated process location
   */
  def with_process_at(process: AbstractProcess[G], position: CFGEntry[G]): AbstractState[G] =
    AbstractState(valuations, processes.removed(process) + (process -> position), lock, seq_lengths)

  /**
   * Updates the state by removing a process from the active list.
   *
   * @param process Process to remove
   * @return An abstract state that is a copy of this one without the given process
   */
  def without_process(process: AbstractProcess[G]): AbstractState[G] =
    AbstractState(valuations, processes.removed(process), lock, seq_lengths)

  /**
   * Updates the state by locking the global lock.
   *
   * @param process Process that should hold the global lock
   * @return An abstract state that is a copy of this one with the lock held by the given process
   */
  def locked_by(process: AbstractProcess[G]): AbstractState[G] = AbstractState(valuations, processes, Some(process), seq_lengths)

  /**
   * Updates the state by unlocking the global lock.
   *
   * @return An abstract state that is a copy of this one with the global lock unlocked
   */
  def unlocked(): AbstractState[G] = AbstractState(valuations, processes, None, seq_lengths)

  /**
   * Updates the state by updating the value of a certain variable.
   *
   * @param variable Variable to update
   * @param value New value for the variable
   * @return An abstract state that is a copy of this one with the valuation for the given variable changed
   */
  def with_valuation(variable: Expr[G], value: UncertainValue): AbstractState[G] = variable_from_expr(variable) match {
    case Some(concrete_variable) => val_updated(concrete_variable, value)
    case None => this
  }

  private def val_updated(variable: ConcreteVariable[G], value: UncertainValue): AbstractState[G] =
    AbstractState(valuations + (variable -> value), processes, lock, seq_lengths)

  /**
   * Updates the state by updating all variables that are affected by an update to a collection.
   *
   * @param variable The collection that should be updated
   * @param assigned New value for the collection
   * @return An abstract state that is a copy of this one with the values of all variables that are affected by the
   *         collection updated accordingly
   */
  def with_updated_collection(variable: Expr[G], assigned: Expr[G]): AbstractState[G] = {
    val affected: Set[IndexedVariable[G]] = valuations.keySet.filter(v => v.is_contained_by(variable, this)).collect{ case v: IndexedVariable[_] => v }
    if (affected.isEmpty) return this
    val by_index: Map[Int, IndexedVariable[G]] = Map.from(affected.map(v => (v.i, v)))
    val new_values: UncertainSequence = get_collection_value(assigned)
    var vals: Map[ConcreteVariable[G], UncertainValue] = valuations
    by_index.foreach(t => vals = vals + (t._2 -> new_values.get(t._1)))
    AbstractState(vals, processes, lock, seq_lengths + (affected.head.field -> new_values.len))
  }

  private def get_collection_value(lit: Expr[G]): UncertainSequence = lit match {
    // Literals
    case LiteralSeq(element, values) =>
      UncertainSequence(UncertainIntegerValue.single(values.size),
                        values.zipWithIndex.map(t => UncertainIntegerValue.single(t._2) -> resolve_expression(t._1)),
                        element)
    case UntypedLiteralSeq(values) =>
      UncertainSequence(UncertainIntegerValue.single(values.size),
                        values.zipWithIndex.map(t => UncertainIntegerValue.single(t._2) -> resolve_expression(t._1)),
                        values.head.t)
    // Variables
    case d: Deref[_] => collection_from_variable(d)
    // TODO: Implement array semantics
    case Values(arr, from, to) => ???
    case NewArray(element, dims, moreDims, initialize) => ???
    // Sequence operations
    case Cons(x, xs) => get_collection_value(xs).prepend(resolve_expression(x))
    case Concat(xs, ys) => get_collection_value(xs).concat(get_collection_value(ys))
    case Drop(xs, count) => get_collection_value(xs).drop(resolve_integer_expression(count))
    case Take(xs, count) => get_collection_value(xs).take(resolve_integer_expression(count))
    case SeqUpdate(xs, i, x) => get_collection_value(xs).updated(resolve_integer_expression(i), resolve_expression(x))
    case RemoveAt(xs, i) => get_collection_value(xs).remove(resolve_integer_expression(i))
    case Slice(xs, from, to) => get_collection_value(xs).slice(resolve_integer_expression(from), resolve_integer_expression(to))
    // Other expressions that can evaluate to a collection
    case Old(expr, _) => get_collection_value(expr)
  }

  private def collection_from_variable(deref: Deref[G]): UncertainSequence = {
    val affected: Set[IndexedVariable[G]] = valuations.keySet.filter(v => v.is_contained_by(deref, this)).collect { case v: IndexedVariable[_] => v }
    val len: Option[UncertainIntegerValue] = seq_lengths.get(deref.ref.decl)
    val t: Type[G] = deref.ref.decl.t match {
      case TArray(element) => element
      case TSeq(element) => element
      case _ => throw new IllegalArgumentException(s"Unsupported collection type ${deref.ref.decl.t.toInlineString}")
    }
    UncertainSequence(len.getOrElse(UncertainIntegerValue.above(affected.map(v => v.i).max)),
                      affected.map(v => UncertainIntegerValue.single(v.i) -> valuations(v)).toSeq,
                      t)
  }

  /**
   * Updates the state by taking a specification in the form of an assumption into account.
   *
   * @param assumption Boolean expression expressing a state update
   * @return A set of abstract states that are a copy of this one, updated according to the given assumption
   */
  def with_assumption(assumption: Expr[G]): Set[AbstractState[G]] =
    resolve_effect(assumption, negate = false).filter(m => !m.is_impossible).map(m => AbstractState(valuations ++ m.resolve, processes, lock, seq_lengths))

  private def resolve_effect(assumption: Expr[G], negate: Boolean): Set[ConstraintMap[G]] = assumption match {
    // Consider boolean/separation logic operators
    case Not(arg) => this.resolve_effect(arg, !negate)
    case AmbiguousOr(left, right) =>
      if (!negate) handle_or(left, right)
      else handle_and(left, right, neg_left = true, neg_right = true)
    case Star(left, right) =>
      if (!negate) handle_and(left, right)
      else handle_or(left, right, neg_left = true, neg_right = true)
    case And(left, right) =>
      if (!negate) handle_and(left, right)
      else handle_or(left, right, neg_left = true, neg_right = true)
    case Or(left, right) =>
      if (!negate) handle_or(left, right)
      else handle_and(left, right, neg_left = true, neg_right = true)
    case Implies(left, right) =>
      if (!negate) handle_or(left, right, neg_left = true)
      else handle_and(left, right, neg_right = true)
    // Atomic comparisons, if they contain a concrete variable, can actually affect the state
    case c: Comparison[G] => handle_update(c, negate)
    // Boolean variables could appear in the assumption without any comparison
    case e: Expr[G] if valuations.keys.exists(v => v.is(e, this)) => Set(ConstraintMap.from(variable_from_expr(e).get, UncertainBooleanValue.from(!negate)))
    case _ => throw new IllegalArgumentException(s"Effect of contract expression ${assumption.toInlineString} is not implemented")
  }

  private def handle_and(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[ConstraintMap[G]] = {
    val left_constraints = resolve_effect(left, neg_left)
    val right_constraints = resolve_effect(right, neg_right)
    left_constraints.flatMap(m1 => right_constraints.map(m2 => m1 ++ m2))
  }

  private def handle_or(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[ConstraintMap[G]] = {
    val pure_left = is_pure(left)
    val pure_right = is_pure(right)
    // If one side is pure and the other has an effect on the state, treat this "or" as an implication. If both sides
    // update the state, treat it as a split in the state space instead
    if (pure_left && pure_right) Set(ConstraintMap.empty[G])
    else if (pure_left) handle_implies(left, right, !neg_left, neg_right)
    else if (pure_right) handle_implies(right, left, !neg_right, neg_left)
    else this.resolve_effect(left, neg_left) ++ this.resolve_effect(right, neg_right)
  }

  private def handle_implies(left: Expr[G], right: Expr[G], neg_left: Boolean = false, neg_right: Boolean = false): Set[ConstraintMap[G]] = {
    val resolve_left: UncertainBooleanValue = resolve_boolean_expression(left)
    var res: Set[ConstraintMap[G]] = Set()
    if (neg_left && resolve_left.can_be_false || (!neg_left) && resolve_left.can_be_true) res = res ++ resolve_effect(right, neg_right)
    if (neg_left && resolve_left.can_be_true || (!neg_left) && resolve_left.can_be_false) res = res ++ Set(ConstraintMap.empty[G])
    res
  }

  private def handle_update(comp: Comparison[G], negate: Boolean): Set[ConstraintMap[G]] = {
    val pure_left = is_pure(comp.left)
    val pure_right = is_pure(comp.right)
    if (pure_left == pure_right) return Set(ConstraintMap.empty[G])
    // Now, exactly one side is pure, and the other contains a concrete variable
    // Resolve the variable and the other side of the equation    TODO: Reduce the side with the variable if it contains anything else as well
    if (valuations.exists(v => v._1.is(if (pure_left) comp.right else comp.left, this))) handle_single_update(comp, pure_left, negate)
    else handle_collection_update(comp, pure_left, negate)
  }

  private def is_pure(node: Node[G]): Boolean = node match {
    // The old value of a variable is always pure, since it cannot be updated
    case _: Old[_] => true
    case e: UnExpr[_] => e.subnodes.forall(n => is_pure(n))
    case e: BinExpr[_] => e.subnodes.forall(n => is_pure(n))
    case e: Expr[_] => if (valuations.keys.exists(v => v.is_contained_by(e, this))) false else e.subnodes.forall(n => is_pure(n))
    case _ => true
  }

  private def handle_single_update(comp: Comparison[G], pure_left: Boolean, negate: Boolean): Set[ConstraintMap[G]] = {
    val variable: ConcreteVariable[G] = if (pure_left) variable_from_expr(comp.right).get else variable_from_expr(comp.left).get
    val value: UncertainValue = if (pure_left) resolve_expression(comp.left) else resolve_expression(comp.right)

    comp match {
      case _: Eq[_] => Set(if (!negate) ConstraintMap.from(variable, value) else ConstraintMap.from(variable, value.complement()))
      case _: Neq[_] => Set(if (!negate) ConstraintMap.from(variable, value.complement()) else ConstraintMap.from(variable, value))
      case AmbiguousGreater(_, _) | Greater(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left == negate, negate)
      case AmbiguousGreaterEq(_, _) | GreaterEq(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left == negate, !negate)
      case AmbiguousLessEq(_, _) | LessEq(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left != negate, !negate)
      case AmbiguousLess(_, _) | Less(_, _) => limit_variable(variable, value.asInstanceOf[UncertainIntegerValue], pure_left != negate, negate)
    }
  }

  private def limit_variable(v: ConcreteVariable[G], b: UncertainIntegerValue, var_greater: Boolean, can_be_equal: Boolean): Set[ConstraintMap[G]] = {
    if (var_greater) {
      if (can_be_equal) Set(ConstraintMap.from(v, b.above_eq()))
      else Set(ConstraintMap.from(v, b.above()))
    }
    else {
      if (can_be_equal) Set(ConstraintMap.from(v, b.below_eq()))
      else Set(ConstraintMap.from(v, b.below()))
    }
  }

  private def handle_collection_update(comp: Comparison[G], pure_left: Boolean, negate: Boolean): Set[ConstraintMap[G]] = {
    val variable: Expr[G] = if (pure_left) comp.right else comp.left
    val value: UncertainSequence = get_collection_value(if (pure_left) comp.left else comp.right)
    val affected: Set[IndexedVariable[G]] = valuations.keySet.filter(v => v.is_contained_by(variable, this)).collect{ case v: IndexedVariable[_] => v }

    comp match {
      case _: Eq[_] if !negate => Set(ConstraintMap.from_cons(affected.map(v => v -> value.get(v.i))))
      case _: Neq[_] if negate => Set(ConstraintMap.from_cons(affected.map(v => v -> value.get(v.i))))
      case _ => throw new IllegalArgumentException(s"The operator ${comp.toInlineString} is not supported for collections")
    }
  }

  /**
   * Updates the state by assuming a postcondition.
   *
   * @param post Postcondition that alters the state
   * @param args A map from the method parameters to the given arguments, to be textually replaced
   * @return A set of abstract states that are a copy of this one after assuming the given postcondition with the given
   *         arguments
   */
  def with_postcondition(post: AccountedPredicate[G], args: Map[Variable[G], Expr[G]]): Set[AbstractState[G]] =
    with_assumption(unify_expression(AstBuildHelpers.unfoldPredicate(post).reduce((e1, e2) => Star(e1, e2)(e1.o)), args))

  private def unify_expression(cond: Expr[G], args: Map[Variable[G], Expr[G]]): Expr[G] =
    Substitute(args.map[Expr[G], Expr[G]]{ case (v, e) => Local[G](v.ref)(v.o) -> Old(e, None)(e.o)(e.o) }).dispatch(cond)

  /**
   * Evaluates an expression and returns an uncertain value, depending on the type of expression and the values it can
   * take with the given level of abstraction.
   *
   * @param expr COL expression to resolve
   * @return An uncertain value of the correct type
   */
  def resolve_expression(expr: Expr[G]): UncertainValue = expr.t match {
    case _: IntType[_] => resolve_integer_expression(expr)
    case _: TBool[_] => resolve_boolean_expression(expr)
    case _ => throw new IllegalArgumentException(s"Type ${expr.t.toInlineString} is not supported")
  }

  /**
   * Evaluates an integer expression and returns an uncertain integer value.
   *
   * @param expr integer-type COL expression
   * @return An uncertain value that represents all possible valuations of the given expression
   */
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
    case Size(obj) => get_collection_value(obj).len
    case ProcedureInvocation(_, _, _, _, _, _) => UncertainIntegerValue.uncertain()   // TODO: return value from procedure/method?
    case MethodInvocation(_, _, _, _, _, _, _) => UncertainIntegerValue.uncertain()
  }

  /**
   * Evaluates a boolean expression and returns an uncertain boolean value.
   *
   * @param expr boolean-type COL expression
   * @return An uncertain boolean value that represents all possible values that the given expression can take on
   */
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

  /**
   * Returns an expression to represent this state of the form <code>variable1 == value1 && variable2 == value2 && ...</code>
   *
   * @return An expression that encodes this state
   */
  def to_expression: Expr[G] = valuations.map(v => v._2.to_expression(v._1.to_expression)).reduce((e1, e2) => And(e1, e2)(e1.o))
}
