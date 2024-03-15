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
    case Some(concrete_variable) => AbstractState(valuations + (variable -> value), processes, lock, seq_lengths)
    case None => this
  }

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
    val new_values: UncertainSequence = resolve_collection_expression(assigned)
    var vals: Map[ConcreteVariable[G], UncertainValue] = valuations
    by_index.foreach(t => vals = vals + (t._2 -> new_values.get(t._1)))
    AbstractState(vals, processes, lock, seq_lengths + (affected.head.field -> new_values.len))
  }


  /**
   * Updates the state by taking a specification in the form of an assumption into account.
   *
   * @param assumption Boolean expression expressing a state update
   * @return A set of abstract states that are a copy of this one, updated according to the given assumption
   */
  def with_assumption(assumption: Expr[G]): Set[AbstractState[G]] = {
    val constraints: Set[ConstraintMap[G]] = new ConstraintSolver(this, valuations.keySet).resolve_assumption(assumption).filter(m => !m.is_impossible)
    constraints.map(m => m.resolve).map(m => AbstractState(valuations.map(e => e._1 -> m.getOrElse(e._1, e._2)), processes, lock, seq_lengths))
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

  /**
   * Evaluates an expression and returns an uncertain value, depending on the type of expression and the values it can
   * take with the given level of abstraction. This method can only handle single-value types, not collections.
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
      if (resolve_boolean_expression(cond).can_be_true) value = value.union(resolve_integer_expression(ift)).asInstanceOf[UncertainIntegerValue]
      if (resolve_boolean_expression(cond).can_be_false) value = value.union(resolve_integer_expression(iff)).asInstanceOf[UncertainIntegerValue]
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
    case Length(arr) => UncertainIntegerValue.above(0)    // TODO: Implement array semantics
    case Size(obj) => resolve_collection_expression(obj).len
    case ProcedureInvocation(ref, args, _, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainIntegerValue]
    case MethodInvocation(_, ref, args, _, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainIntegerValue]
    case FunctionInvocation(ref, args, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainIntegerValue]
    case InstanceFunctionInvocation(_, ref, args, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainIntegerValue]
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
      if (resolve_boolean_expression(cond).can_be_true) value = value.union(resolve_boolean_expression(ift)).asInstanceOf[UncertainBooleanValue]
      if (resolve_boolean_expression(cond).can_be_false) value = value.union(resolve_boolean_expression(iff)).asInstanceOf[UncertainBooleanValue]
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
    case ProcedureInvocation(ref, args, _, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainBooleanValue]
    case MethodInvocation(_, ref, args, _, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainBooleanValue]
    case FunctionInvocation(ref, args, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainBooleanValue]
    case InstanceFunctionInvocation(_, ref, args, _, _, _) =>
      get_subroutine_return(ref.decl.contract.ensures, Map.from(ref.decl.args.zip(args)), ref.decl.returnType).asInstanceOf[UncertainBooleanValue]
  }

  /**
   * Evaluates a collection expression and returns an uncertain collection value.
   *
   * @param expr collection-type COL expression
   * @return An uncertain collection value that represents all possible values that the given expression can take on,
   *         possibly of uncertain length and with uncertain values at uncertain indices
   */
  def resolve_collection_expression(expr: Expr[G]): UncertainSequence = expr match {
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
    case Cons(x, xs) => resolve_collection_expression(xs).prepend(resolve_expression(x))
    case Concat(xs, ys) => resolve_collection_expression(xs).concat(resolve_collection_expression(ys))
    case Drop(xs, count) => resolve_collection_expression(xs).drop(resolve_integer_expression(count))
    case Take(xs, count) => resolve_collection_expression(xs).take(resolve_integer_expression(count))
    case SeqUpdate(xs, i, x) => resolve_collection_expression(xs).updated(resolve_integer_expression(i), resolve_expression(x))
    case RemoveAt(xs, i) => resolve_collection_expression(xs).remove(resolve_integer_expression(i))
    case Slice(xs, from, to) => resolve_collection_expression(xs).slice(resolve_integer_expression(from), resolve_integer_expression(to))
    // Other expressions that can evaluate to a collection
    case Select(cond, ift, iff) =>
      val condition: UncertainBooleanValue = resolve_boolean_expression(cond)
      val ift_seq: UncertainSequence = resolve_collection_expression(ift)
      val iff_seq: UncertainSequence = resolve_collection_expression(iff)
      if (condition.can_be_true && condition.can_be_false) ift_seq.union(iff_seq)
      else if (condition.can_be_true) ift_seq
      else if (condition.can_be_false) iff_seq
      else UncertainSequence.empty(ift_seq.t)
    case Old(expr, _) => resolve_collection_expression(expr)
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

  private def get_subroutine_return(post: AccountedPredicate[G], args: Map[Variable[G], Expr[G]], return_type: Type[G]): UncertainValue =
    get_return(unify_expression(AstBuildHelpers.unfoldPredicate(post).reduce((e1, e2) => Star(e1, e2)(e1.o)), args), return_type)

  private def get_return(contract: Expr[G], return_type: Type[G]): UncertainValue = {
    val result_var: ResultVariable[G] = ResultVariable()
    val constraints: Set[ConstraintMap[G]] = new ConstraintSolver(this, Set(result_var)).resolve_assumption(contract).filter(m => !m.is_impossible)
    val possible_vals: Set[UncertainValue] = constraints.map(m => m.resolve.getOrElse(result_var, UncertainValue.uncertain_of(return_type)))
    possible_vals.reduce((v1, v2) => v1.union(v2))
  }

  private def unify_expression(cond: Expr[G], args: Map[Variable[G], Expr[G]]): Expr[G] =
    Substitute(args.map[Expr[G], Expr[G]]{ case (v, e) => Local[G](v.ref)(v.o) -> Old(e, None)(e.o)(e.o) }).dispatch(cond)

  private def variable_from_expr(variable: Expr[G]): Option[ConcreteVariable[G]] =
    valuations.keys.collectFirst{ case c: ConcreteVariable[G] if c.is(variable, this) => c }

  /**
   * Returns an expression to represent this state of the form <code>variable1 == value1 && variable2 == value2 && ...</code>
   *
   * @return An expression that encodes this state
   */
  def to_expression: Expr[G] = valuations.map(v => v._2.to_expression(v._1.to_expression)).reduce((e1, e2) => And(e1, e2)(e1.o))
}
