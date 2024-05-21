package vct.rewrite.rasi

import vct.col.ast._
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
    processes.keySet.filter(p => lock.isEmpty || lock.get.equals(p)).flatMap(p => p.atomic_step(this))

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
    case Some(concrete_variable) => AbstractState(valuations + (concrete_variable -> value), processes, lock, seq_lengths)
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
  def with_assumption(assumption: Expr[G], is_contract: Boolean = false): Set[AbstractState[G]] = {
    // TODO: An assumption adds constraints to the existing state! A postcondition first havocs all variables for which it has permission.
    val constraints: Set[ConstraintMap[G]] = new ConstraintSolver(this, valuations.keySet, is_contract).resolve_assumption(assumption).filter(m => !m.is_impossible)
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
    with_assumption(Utils.unify_expression(Utils.contract_to_expression(post), args), is_contract = true)

  /**
   * Evaluates an expression and returns an uncertain value, depending on the type of expression and the values it can
   * take with the given level of abstraction. This method can only handle single-value types, not collections.
   *
   * @param expr COL expression to resolve
   * @return An uncertain value of the correct type
   */
  def resolve_expression(expr: Expr[G], is_old: Boolean = false, is_contract: Boolean = false): UncertainValue = expr.t match {
    case _: IntType[_] => resolve_integer_expression(expr, is_old, is_contract)
    case _: TBool[_] => resolve_boolean_expression(expr, is_old, is_contract)
    case _ => throw new IllegalArgumentException(s"Type ${expr.t.toInlineString} is not supported")
  }

  /**
   * Evaluates an integer expression and returns an uncertain integer value.
   *
   * @param expr integer-type COL expression
   * @return An uncertain value that represents all possible valuations of the given expression
   */
  def resolve_integer_expression(expr: Expr[G], is_old: Boolean = false, is_contract: Boolean = false): UncertainIntegerValue = expr match {
    case CIntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case IntegerValue(value) => UncertainIntegerValue.single(value.intValue)
    case SizeOf(tname) => UncertainIntegerValue.above(1)    // TODO: Can we use more information about sizeof?
    case UMinus(arg) => -resolve_integer_expression(arg, is_old, is_contract)
    case AmbiguousMult(left, right) => resolve_integer_expression(left, is_old, is_contract) * resolve_integer_expression(right, is_old, is_contract)
    case AmbiguousPlus(left, right) => resolve_integer_expression(left, is_old, is_contract) + resolve_integer_expression(right, is_old, is_contract)
    case AmbiguousMinus(left, right) => resolve_integer_expression(left, is_old, is_contract) - resolve_integer_expression(right, is_old, is_contract)
    case Exp(left, right) => resolve_integer_expression(left, is_old, is_contract).pow(resolve_integer_expression(right, is_old, is_contract))
    case Plus(left, right) => resolve_integer_expression(left, is_old, is_contract) + resolve_integer_expression(right, is_old, is_contract)
    case Minus(left, right) => resolve_integer_expression(left, is_old, is_contract) - resolve_integer_expression(right, is_old, is_contract)
    case Mult(left, right) => resolve_integer_expression(left, is_old, is_contract) * resolve_integer_expression(right, is_old, is_contract)
    case FloorDiv(left, right) => resolve_integer_expression(left, is_old, is_contract) / resolve_integer_expression(right, is_old, is_contract)
    case Mod(left, right) => resolve_integer_expression(left, is_old, is_contract) % resolve_integer_expression(right, is_old, is_contract)
    // Bit operations destroy any knowledge of integer state       TODO: Support bit operations?
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
    case Select(cond, ift, iff) =>
      var value: UncertainIntegerValue = UncertainIntegerValue.empty()
      if (resolve_boolean_expression(cond, is_old, is_contract).can_be_true) {
        value = value.union(resolve_integer_expression(ift, is_old, is_contract)).asInstanceOf[UncertainIntegerValue]
      }
      if (resolve_boolean_expression(cond, is_old, is_contract).can_be_false) {
        value = value.union(resolve_integer_expression(iff, is_old, is_contract)).asInstanceOf[UncertainIntegerValue]
      }
      value
    case Old(exp, at) => at match {
      case Some(_) => throw new IllegalArgumentException(s"Cannot resolve labelled old expression ${expr.toInlineString}")
      case None => resolve_integer_expression(exp, is_old = true, is_contract)
    }
    case Local(_) | DerefHeapVariable(_) | Deref(_, _) | DerefPointer(_) | AmbiguousSubscript(_, _) | SeqSubscript(_, _) | ArraySubscript(_, _) | PointerSubscript(_, _) =>
      variable_from_expr(expr) match {
        case Some(v) =>
          if (is_contract && !is_old) UncertainIntegerValue.uncertain()
          else valuations(v).asInstanceOf[UncertainIntegerValue]
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
    case Result(_) => UncertainIntegerValue.uncertain()
    case AmbiguousResult() => UncertainIntegerValue.uncertain()
  }

  /**
   * Evaluates a boolean expression and returns an uncertain boolean value.
   *
   * @param expr boolean-type COL expression
   * @return An uncertain boolean value that represents all possible values that the given expression can take on
   */
  def resolve_boolean_expression(expr: Expr[G], is_old: Boolean = false, is_contract: Boolean = false): UncertainBooleanValue = expr match {
    case BooleanValue(value) => UncertainBooleanValue.from(value)
    case Not(arg) => !resolve_boolean_expression(arg)
    case AmbiguousOr(left, right) => resolve_boolean_expression(left, is_old, is_contract) || resolve_boolean_expression(right, is_old, is_contract)
    case Star(left, right) => resolve_boolean_expression(left, is_old, is_contract) && resolve_boolean_expression(right, is_old, is_contract)
    case And(left, right) => resolve_boolean_expression(left, is_old, is_contract) && resolve_boolean_expression(right, is_old, is_contract)
    case Or(left, right) => resolve_boolean_expression(left, is_old, is_contract) || resolve_boolean_expression(right, is_old, is_contract)
    case Implies(left, right) => (!resolve_boolean_expression(left, is_old, is_contract)) || resolve_boolean_expression(right, is_old, is_contract)
    case Eq(left, right) => handle_equality(left, right, is_old, is_contract)
    case Neq(left, right) => !handle_equality(left, right, is_old, is_contract)
    case AmbiguousGreater(left, right) => resolve_integer_expression(left, is_old, is_contract) > resolve_integer_expression(right, is_old, is_contract)
    case AmbiguousLess(left, right) => resolve_integer_expression(left, is_old, is_contract) < resolve_integer_expression(right, is_old, is_contract)
    case AmbiguousGreaterEq(left, right) => resolve_integer_expression(left, is_old, is_contract) >= resolve_integer_expression(right, is_old, is_contract)
    case AmbiguousLessEq(left, right) => resolve_integer_expression(left, is_old, is_contract) <= resolve_integer_expression(right, is_old, is_contract)
    case Greater(left, right) => resolve_integer_expression(left, is_old, is_contract) > resolve_integer_expression(right, is_old, is_contract)
    case Less(left, right) => resolve_integer_expression(left, is_old, is_contract) < resolve_integer_expression(right, is_old, is_contract)
    case GreaterEq(left, right) => resolve_integer_expression(left, is_old, is_contract) >= resolve_integer_expression(right, is_old, is_contract)
    case LessEq(left, right) => resolve_integer_expression(left, is_old, is_contract) <= resolve_integer_expression(right, is_old, is_contract)
    case Select(cond, ift, iff) =>
      var value: UncertainBooleanValue = UncertainBooleanValue.empty()
      if (resolve_boolean_expression(cond, is_old, is_contract).can_be_true) {
        value = value.union(resolve_boolean_expression(ift, is_old, is_contract)).asInstanceOf[UncertainBooleanValue]
      }
      if (resolve_boolean_expression(cond, is_old, is_contract).can_be_false) {
        value = value.union(resolve_boolean_expression(iff, is_old, is_contract)).asInstanceOf[UncertainBooleanValue]
      }
      value
    case Old(exp, at) => at match {
      case Some(_) => throw new IllegalArgumentException(s"Cannot resolve labelled old expression ${expr.toInlineString}")
      case None => resolve_boolean_expression(exp, is_old = true, is_contract)
    }
    case Local(_) | DerefHeapVariable(_) | Deref(_, _) | DerefPointer(_) | AmbiguousSubscript(_, _) | SeqSubscript(_, _) | ArraySubscript(_, _) | PointerSubscript(_, _) =>
      variable_from_expr(expr) match {
        case Some(v) =>
          if (is_contract && !is_old) UncertainBooleanValue.uncertain()
          else valuations(v).asInstanceOf[UncertainBooleanValue]
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
    case Held(_) => UncertainBooleanValue.from(lock.nonEmpty)   // TODO: This means that ANY process holds the lock!
    case Scale(_, res) => resolve_boolean_expression(res, is_old, is_contract)   // TODO: Do anything with permission fraction?
    case PredicateApply(ref, args, _) =>                    // TODO: Do anything with permission fraction?
      if (ref.decl.body.nonEmpty) UncertainBooleanValue.from(true)   //resolve_boolean_expression(Utils.unify_expression(ref.decl.body.get, Map.from(ref.decl.args.zip(args))))
      else ???    // TODO: Track resource ownership?
    case InstancePredicateApply(_, ref, args, _) =>         // TODO: Do anything with permission fraction?
      if (ref.decl.body.nonEmpty) UncertainBooleanValue.from(true)   //resolve_boolean_expression(Utils.unify_expression(ref.decl.body.get, Map.from(ref.decl.args.zip(args))))
      else ???    // TODO: Track resource ownership?
    case Perm(_, _) => UncertainBooleanValue.from(true)     // TODO: Do anything with permissions/resources?
    case _: Binder[_] => UncertainBooleanValue.from(true)   // TODO: How to handle quantifiers?!
    case Result(_) => UncertainBooleanValue.uncertain()
    case AmbiguousResult() => UncertainBooleanValue.uncertain()
  }

  /**
   * Evaluates a collection expression and returns an uncertain collection value.
   *
   * @param expr collection-type COL expression
   * @return An uncertain collection value that represents all possible values that the given expression can take on,
   *         possibly of uncertain length and with uncertain values at uncertain indices
   */
  def resolve_collection_expression(expr: Expr[G], is_old: Boolean = false, is_contract: Boolean = false): UncertainSequence = expr match {
    // Literals
    case LiteralSeq(element, values) =>
      UncertainSequence(UncertainIntegerValue.single(values.size),
        values.zipWithIndex.map(t => UncertainIntegerValue.single(t._2) -> resolve_expression(t._1, is_old, is_contract)),
        element)
    case UntypedLiteralSeq(values) =>
      UncertainSequence(UncertainIntegerValue.single(values.size),
        values.zipWithIndex.map(t => UncertainIntegerValue.single(t._2) -> resolve_expression(t._1, is_old, is_contract)),
        values.head.t)
    // Variables
    case d: Deref[_] => collection_from_variable(d, is_old, is_contract)
    // TODO: Implement array semantics
    case Values(arr, from, to) => ???
    case NewArray(element, dims, moreDims, initialize) => ???
    // Sequence operations
    case Cons(x, xs) =>
      resolve_collection_expression(xs, is_old, is_contract).prepend(resolve_expression(x, is_old, is_contract))
    case AmbiguousPlus(xs, ys) =>
      resolve_collection_expression(xs, is_old, is_contract).concat(resolve_collection_expression(ys, is_old, is_contract))
    case Concat(xs, ys) =>
      resolve_collection_expression(xs, is_old, is_contract).concat(resolve_collection_expression(ys, is_old, is_contract))
    case Drop(xs, count) =>
      resolve_collection_expression(xs, is_old, is_contract).drop(resolve_integer_expression(count, is_old, is_contract))
    case Take(xs, count) =>
      resolve_collection_expression(xs, is_old, is_contract).take(resolve_integer_expression(count, is_old, is_contract))
    case SeqUpdate(xs, i, x) =>
      resolve_collection_expression(xs, is_old, is_contract).updated(resolve_integer_expression(i, is_old, is_contract),
                                                                     resolve_expression(x, is_old, is_contract))
    case RemoveAt(xs, i) =>
      resolve_collection_expression(xs, is_old, is_contract).remove(resolve_integer_expression(i, is_old, is_contract))
    case Slice(xs, from, to) =>
      resolve_collection_expression(xs, is_old, is_contract).slice(resolve_integer_expression(from, is_old, is_contract),
                                                                   resolve_integer_expression(to, is_old, is_contract))
    // Other expressions that can evaluate to a collection
    case Select(cond, ift, iff) =>
      val condition: UncertainBooleanValue = resolve_boolean_expression(cond, is_old, is_contract)
      val ift_seq: UncertainSequence = resolve_collection_expression(ift, is_old, is_contract)
      val iff_seq: UncertainSequence = resolve_collection_expression(iff, is_old, is_contract)
      if (condition.can_be_true && condition.can_be_false) ift_seq.union(iff_seq)
      else if (condition.can_be_true) ift_seq
      else if (condition.can_be_false) iff_seq
      else UncertainSequence.empty(ift_seq.t)
    case Old(expr, _) => resolve_collection_expression(expr, is_old = true, is_contract)
  }

  private def collection_from_variable(deref: Deref[G], is_old: Boolean, is_contract: Boolean): UncertainSequence = {
    val affected: Set[IndexedVariable[G]] = valuations.keySet.filter(v => v.is_contained_by(deref, this)).collect { case v: IndexedVariable[_] => v }
    val len: Option[UncertainIntegerValue] = seq_lengths.get(deref.ref.decl)
    val t: Type[G] = deref.ref.decl.t match {
      case TArray(element) => element
      case TSeq(element) => element
      case _ => throw new IllegalArgumentException(s"Unsupported collection type ${deref.ref.decl.t.toInlineString}")
    }
    if (is_contract && !is_old) UncertainSequence.uncertain(t)
    else UncertainSequence(len.getOrElse(UncertainIntegerValue.above(if (affected.isEmpty) 0 else affected.map(v => v.i).max)),
                           affected.map(v => UncertainIntegerValue.single(v.i) -> valuations(v)).toSeq,
                           t)
  }

  private def get_subroutine_return(post: AccountedPredicate[G], args: Map[Variable[G], Expr[G]], return_type: Type[G]): UncertainValue =
    get_return(Utils.unify_expression(Utils.contract_to_expression(post), args), return_type)

  private def get_return(contract: Expr[G], return_type: Type[G]): UncertainValue = {
    val result_var: ResultVariable[G] = ResultVariable(return_type)
    val result_set: Set[ResolvableVariable[G]] = Set(result_var)
    val constraints: Set[ConstraintMap[G]] = new ConstraintSolver(this, result_set, true).resolve_assumption(contract).filter(m => !m.is_impossible)
    val possible_vals: Set[UncertainValue] = constraints.map(m => m.resolve.getOrElse(result_var, UncertainValue.uncertain_of(return_type)))
    possible_vals.reduce((v1, v2) => v1.union(v2))
  }

  private def handle_equality(left: Expr[G], right: Expr[G], is_old: Boolean, is_contract: Boolean): UncertainBooleanValue = left.t match {
    case _: IntType[_] | TBool() => resolve_expression(left, is_old, is_contract) == resolve_expression(right, is_old, is_contract)
    case _: TSeq[_] | TArray(_) => resolve_collection_expression(left, is_old, is_contract) == resolve_collection_expression(right, is_old, is_contract)
    case _ => UncertainBooleanValue.from(true)    // TODO: The justification for this is that the program will be verified anyway,
                                                  //  so object equality does not need to be considered; does that make sense?
  }

  private def variable_from_expr(variable: Expr[G]): Option[ConcreteVariable[G]] =
    valuations.keys.collectFirst{ case c: ConcreteVariable[G] if c.is(variable, this) => c }

  /**
   * Returns an expression to represent this state of the form <code>variable1 == value1 && variable2 == value2 && ...</code>
   *
   * @return An expression that encodes this state
   */
  def to_expression: Expr[G] = valuations.map(v => v._2.to_expression(v._1.to_expression)).reduce((e1, e2) => And(e1, e2)(e1.o))
}
