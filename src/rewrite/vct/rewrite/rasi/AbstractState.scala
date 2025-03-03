package vct.rewrite.rasi

import vct.col.ast._
import vct.rewrite.cfg.CFGEntry

import scala.collection.immutable.HashMap

case class AbstractState[G](
    valuations: Map[FieldVariable[G], UncertainSingleValue],
    processes: HashMap[AbstractProcess[G], CFGEntry[G]],
    local: Map[LocalVariable[G], UncertainSingleValue],
    local_dependencies: Map[Variable[G], Set[FieldVariable[G]]],
    lock: Option[AbstractProcess[G]],
    parameters: Map[FieldSimpleVariable[G], UncertainSingleValue],
    tracked_sequences: Map[InstanceField[G], Set[FieldVariable[G]]],
) {

  /** Main function of the abstract state. For all processes that could
    * potentially run, execute all possible next steps.
    *
    * @return
    *   The set of possible successor states
    */
  def successors(): RASISuccessor[G] =
    RASISuccessor(
      processes.keySet.filter(p => lock.isEmpty || lock.get.equals(p))
        .map(p => p.atomic_step(this))
    )

  /** Returns a state with the same tracked variables, but with no knowledge of
    * their values.
    *
    * @return
    *   A copy of this state with all variable values perfectly uncertain
    */
  def reset: AbstractState[G] = {
    AbstractState(
      valuations.map(v => v._1 -> UncertainSingleValue.uncertain_of(v._1.t)),
      processes,
      Map.empty[LocalVariable[G], UncertainSingleValue],
      Map.empty[Variable[G], Set[FieldVariable[G]]],
      lock,
      parameters,
      tracked_sequences,
    )
  }

  /** Returns the same state except with all knowledge of local variables
    * cleared.
    *
    * @return
    *   A copy of this state without the knowledge of local variables
    */
  def reset_locals: AbstractState[G] =
    AbstractState(
      valuations,
      processes,
      Map.empty[LocalVariable[G], UncertainSingleValue],
      Map.empty[Variable[G], Set[FieldVariable[G]]],
      lock,
      parameters,
      tracked_sequences,
    )

  /** Updates the state by changing the program counter for a process.
    *
    * @param process
    *   Process to update
    * @param position
    *   New position of the process
    * @return
    *   An abstract state that is a copy of this one with the updated process
    *   location
    */
  def with_process_at(
      process: AbstractProcess[G],
      position: CFGEntry[G],
  ): AbstractState[G] =
    AbstractState(
      valuations,
      processes.removed(process) + (process -> position),
      local,
      local_dependencies,
      lock,
      parameters,
      tracked_sequences,
    )

  /** Updates the state by removing a process from the active list.
    *
    * @param process
    *   Process to remove
    * @return
    *   An abstract state that is a copy of this one without the given process
    */
  def without_process(process: AbstractProcess[G]): AbstractState[G] =
    AbstractState(
      valuations,
      processes.removed(process),
      local,
      local_dependencies,
      lock,
      parameters,
      tracked_sequences,
    )

  /** Updates the state by locking the global lock.
    *
    * @param process
    *   Process that should hold the global lock
    * @return
    *   An abstract state that is a copy of this one with the lock held by the
    *   given process
    */
  def locked_by(process: AbstractProcess[G]): AbstractState[G] =
    AbstractState(
      valuations,
      processes,
      local,
      local_dependencies,
      Some(process),
      parameters,
      tracked_sequences,
    )

  /** Updates the state by unlocking the global lock.
    *
    * @return
    *   An abstract state that is a copy of this one with the global lock
    *   unlocked
    */
  def unlocked(): AbstractState[G] =
    AbstractState(
      valuations,
      processes,
      local,
      local_dependencies,
      None,
      parameters,
      tracked_sequences,
    )

  /** Splits this state such that every variable for which this is possible only
    * has a single value in any of the resulting states. Variables for which
    * this is not possible have their uncertain value copied into all substates.
    *
    * @return
    *   A set of states that, in total, represents the same valuations as this
    *   state, with each resulting state containing only variable valuations
    *   that are as sharp as possible
    */
  def split_values(): Set[AbstractState[G]] = {
    val valuation_sets
        : Iterable[Set[(ConcreteVariable[G], UncertainSingleValue)]] =
      valuations.map(t => t._2.split.getOrElse(Set(t._2)).map(v => t._1 -> v))
    Utils.cartesian_product(valuation_sets).map(vs =>
      AbstractState(
        Utils
          .cast_resolvable_map[G, ConcreteVariable[G], FieldVariable[
            G
          ], UncertainSingleValue](Map.from(vs)),
        processes,
        local,
        local_dependencies,
        lock,
        parameters,
        tracked_sequences,
      )
    )
  }

  /** Updates the state by adding a path condition to its knowledge of
    * parameters (to avoid infeasible assumptions about potential paths).
    *
    * @param cond
    *   Path condition; can be <code>None</code>, which is equivalent to
    *   <code>true</code>
    * @return
    *   An abstract state that is a copy of this one with the path condition
    *   taken into account
    */
  def with_condition(cond: Option[Expr[G]]): AbstractState[G] =
    cond match {
      case None => this
      case Some(expr) =>
        val c: Map[ResolvableVariable[G], UncertainSingleValue] =
          new ConstraintSolver(
            this,
            valuations.keySet
              .union(parameters.keySet.asInstanceOf[Set[FieldVariable[G]]]),
            false,
          ).resolve_assumption(expr).filter(m => !m.is_impossible)
            .reduce((m1, m2) => m1 || m2).resolve
            .map(t => t._1 -> t._2.asInstanceOf[UncertainSingleValue])
        AbstractState(
          valuations.map(v =>
            v._1 ->
              (if (c.contains(v._1))
                 v._2.intersection(c(v._1)).asInstanceOf[UncertainSingleValue]
               else
                 v._2)
          ),
          processes,
          local,
          local_dependencies,
          lock,
          parameters.map(v =>
            v._1 ->
              (if (c.contains(v._1))
                 v._2.intersection(c(v._1)).asInstanceOf[UncertainSingleValue]
               else
                 v._2)
          ),
          tracked_sequences,
        )
    }

  /** Resolves the effect of an assertion, like a loop invariant, on the
    * knowledge about local variables.
    *
    * @param cond
    *   Expression encoding the assertion to be processed
    * @return
    *   A copy of this state with local variables updated according to the
    *   condition
    */
  def with_local_condition(cond: Expr[G]): AbstractState[G] = {
    val c: Map[ResolvableVariable[G], UncertainSingleValue] =
      new ConstraintSolver(
        this,
        cond.collect {
          case l: Local[_] if (l.t match {
                case _: IntType[_] | _: TBool[_] | _: TSeq[_] => true
                case _ => false
              }) =>
            l
        }.map(l => get_local_var(l)).toSet,
        false,
      ).resolve_assumption(cond).filter(m => !m.is_impossible)
        .reduce((m1, m2) => m1 || m2).resolve
        .map(t => t._1 -> t._2.asInstanceOf[UncertainSingleValue])
    AbstractState(
      valuations,
      processes,
      Utils
        .cast_resolvable_map[G, ResolvableVariable[G], LocalVariable[
          G
        ], UncertainSingleValue](c),
      local_dependencies, // TODO: Which untracked variables influence the locals?
      lock,
      parameters,
      tracked_sequences,
    )
  }

  /** Updates the state by updating the value of a certain variable.
    *
    * @param variable
    *   Variable to update
    * @param value
    *   New value for the variable
    * @return
    *   An abstract state that is a copy of this one with the valuation for the
    *   given variable changed
    */
  def with_valuation(
      variable: Expr[G],
      value: UncertainSingleValue,
  ): AbstractState[G] =
    variable match {
      case l: Local[_] =>
        AbstractState(
          valuations,
          processes,
          local + (get_local_var(l) -> value),
          local_dependencies,
          lock,
          parameters,
          tracked_sequences,
        )
      case _ =>
        variable_from_expr(variable) match {
          case Some(concrete_variable) =>
            AbstractState(
              valuations + (concrete_variable -> value),
              processes,
              local,
              local_dependencies,
              lock,
              parameters,
              tracked_sequences,
            )
          case None => this
        }
    }

  /** Updates the state by adding a different valuation map to override or
    * expand the valuations of this state. This is equivalent to calling
    * <code>with_valuation</code> repeatedly with the entries of the new map.
    *
    * @param vals
    *   A new valuation map
    * @return
    *   An abstract state that is a copy of this one with the updated valuation
    */
  def with_new_valuation(
      vals: Map[FieldVariable[G], UncertainSingleValue]
  ): AbstractState[G] =
    AbstractState(
      valuations ++ vals,
      processes,
      local,
      local_dependencies,
      lock,
      parameters,
      tracked_sequences,
    )

  /** Updates the state by updating all variables that are affected by an update
    * to a collection.
    *
    * @param variable
    *   The collection that should be updated
    * @param assigned
    *   New value for the collection
    * @return
    *   An abstract state that is a copy of this one with the values of all
    *   variables that are affected by the collection updated accordingly
    */
  def with_updated_collection(
      variable: Expr[G],
      assigned: Expr[G],
  ): AbstractState[G] =
    variable match {
      case Local(ref) => local_updated_collection(ref.decl, assigned)
      case d: Deref[G] =>
        if (tracked_sequences.contains(d.ref.decl))
          with_updated_tracked_sequence(d, assigned)
        else
          with_updated_collection_entries(d, assigned)
    }

  private def with_updated_tracked_sequence(
      variable: Deref[G],
      assigned: Expr[G],
  ): AbstractState[G] = {
    val value: UncertainSequence = resolve_collection_expression(assigned)
    val target: InstanceField[G] = variable.ref.decl
    val new_values: Map[FieldVariable[G], UncertainSingleValue] =
      Map.from( // TODO: Should something be done with uncertain indices...?
        value.certain_entries
          .map(t => FieldIndexedVariable(target, t._1) -> t._2)
      ).asInstanceOf[Map[FieldVariable[G], UncertainSingleValue]] +
        (FieldSizeVariable(target) -> value.len)
    AbstractState(
      valuations.removedAll(tracked_sequences(target)) ++ new_values,
      processes,
      local,
      local_dependencies,
      lock,
      parameters,
      tracked_sequences + (target -> new_values.keySet),
    )
  }

  private def with_updated_collection_entries(
      variable: Deref[G],
      assigned: Expr[G],
  ): AbstractState[G] = {
    val affected: Set[FieldVariable[G]] = valuations.keySet
      .filter(v => v.is_contained_by(variable, this))
    val indexed: Set[FieldIndexedVariable[G]] = affected.collect {
      case v: FieldIndexedVariable[G] => v
    }
    val size: Set[FieldSizeVariable[G]] = affected.collect {
      case v: FieldSizeVariable[G] => v
    }
    if (affected.isEmpty)
      return this
    val by_index: Map[Int, FieldIndexedVariable[G]] = Map
      .from(indexed.map(v => (v.i, v)))
    val new_values: UncertainSequence = resolve_collection_expression(assigned)
    var vals: Map[FieldVariable[G], UncertainSingleValue] = valuations
    by_index.foreach(t => vals = vals + (t._2 -> new_values.get(t._1)))
    size.foreach(t => vals = vals + (t -> new_values.len))
    AbstractState(
      vals,
      processes,
      local,
      local_dependencies,
      lock,
      parameters,
      tracked_sequences,
    )
  }

  private def local_updated_collection(
      target: Variable[G],
      assigned: Expr[G],
  ): AbstractState[G] = {
    val value: UncertainSequence = resolve_collection_expression(assigned)
    val new_values: Map[LocalVariable[G], UncertainSingleValue] =
      Map.from( // TODO: Should something be done with uncertain indices...?
        value.certain_entries
          .map(t => LocalIndexedVariable(target, t._1) -> t._2)
      ).asInstanceOf[Map[LocalVariable[G], UncertainSingleValue]] +
        (LocalSizeVariable(target) -> value.len)
    AbstractState(
      valuations,
      processes,
      local.filter(t => !t._1.v.equals(target)) ++ new_values,
      local_dependencies, // TODO: Which untracked variables influence the locals?
      lock,
      parameters,
      tracked_sequences,
    )
  }

  /** Updates the state by taking a specification in the form of an assumption
    * into account. Also returns the variables that could cause nondeterministic
    * overapproximation in this operation.
    *
    * @param assumption
    *   Boolean expression expressing a state update
    * @return
    *   A descriptor for states that comply with the given assumption, given
    *   this state as the pre-state
    */
  def with_assumption(assumption: Expr[G]): RASISuccessor[G] = {
    val constraints: Set[Map[FieldVariable[G], UncertainSingleValue]] =
      new ConstraintSolver(
        this,
        valuations.keySet ++ local.keySet,
        is_contract = false,
      ).resolve_assumption(assumption).filter(m => !m.is_impossible).map(m =>
        Utils
          .cast_resolvable_map[G, ResolvableVariable[G], FieldVariable[
            G
          ], UncertainSingleValue](m.resolve.map(t =>
            t._1 -> t._2.asInstanceOf[UncertainSingleValue]
          ))
      ).filter(m =>
        m.forall(t => !t._2.intersection(valuations(t._1)).is_impossible)
      )

    val variables: Set[FieldVariable[G]] = new VariableSelector(this)
      .distinguishing_variables(constraints, Some(assumption))

    RASISuccessor(
      variables,
      constraints.map(m =>
        AbstractState(
          Utils.val_intersect(valuations, m),
          processes,
          local, // TODO: Should the locals also be included in the assumption?
          local_dependencies, // TODO: Which untracked variables influence the locals?
          lock,
          parameters,
          tracked_sequences,
        )
      ),
    )

  }

  /** Updates the state by assuming a postcondition. Also returns the variables
    * that could cause nondeterministic overapproximation in this operation
    *
    * @param post
    *   Postcondition that alters the state
    * @param args
    *   A map from the method parameters to the given arguments, to be textually
    *   replaced
    * @return
    *   A descriptor for states that comply with the given postcondition, given
    *   this state as the pre-state
    */
  def with_postcondition(
      post: AccountedPredicate[G],
      args: Map[Variable[G], Expr[G]],
  ): RASISuccessor[G] = {
    val assumption: Expr[G] = Utils
      .unify_expression(Utils.contract_to_expression(post), args)
    val constraints: Set[Map[FieldVariable[G], UncertainSingleValue]] =
      new ConstraintSolver(this, valuations.keySet, is_contract = true)
        .resolve_assumption(assumption).filter(m => !m.is_impossible).map(m =>
          Utils
            .cast_resolvable_map[G, ResolvableVariable[G], FieldVariable[
              G
            ], UncertainSingleValue](m.resolve.map(t =>
              t._1 -> t._2.asInstanceOf[UncertainSingleValue]
            ))
        )

    val variables: Set[FieldVariable[G]] = new VariableSelector(this)
      .distinguishing_variables(constraints, Some(assumption))

    RASISuccessor(
      variables,
      // A postcondition simply overwrites the values it specifies
      constraints.map(m =>
        AbstractState(
          valuations.map(e => e._1 -> m.getOrElse(e._1, e._2)),
          processes,
          local,
          local_dependencies,
          lock,
          parameters,
          tracked_sequences,
        )
      ),
    )
  }

  def resolve_expression(
      expr: Expr[G],
      is_old: Boolean = false,
      is_contract: Boolean = false,
  ): UncertainValue =
    expr.t match {
      case TSeq(_) | TArray(_) =>
        resolve_collection_expression(expr, is_old, is_contract)
      case _ => resolve_single_expression(expr, is_old, is_contract)
    }

  /** Evaluates an expression and returns an uncertain value, depending on the
    * type of expression and the values it can take with the given level of
    * abstraction. This method can only handle single-value types, not
    * collections.
    *
    * @param expr
    *   COL expression to resolve
    * @return
    *   An uncertain value of the correct type
    */
  def resolve_single_expression(
      expr: Expr[G],
      is_old: Boolean = false,
      is_contract: Boolean = false,
  ): UncertainSingleValue =
    expr.t match {
      case _: IntType[_] =>
        resolve_integer_expression(expr, is_old, is_contract)
      case _: TBool[_] | _: TResource[_] =>
        resolve_boolean_expression(expr, is_old, is_contract)
      case _ =>
        throw new IllegalArgumentException(
          s"Type ${expr.t.toInlineString} is not supported"
        )
    }

  /** Evaluates an integer expression and returns an uncertain integer value.
    *
    * @param expr
    *   integer-type COL expression
    * @return
    *   An uncertain value that represents all possible valuations of the given
    *   expression
    */
  def resolve_integer_expression(
      expr: Expr[G],
      is_old: Boolean = false,
      is_contract: Boolean = false,
  ): UncertainIntegerValue =
    expr match {
      case CIntegerValue(value) => UncertainIntegerValue.single(value.intValue)
      case IntegerValue(value) => UncertainIntegerValue.single(value.intValue)
      case SizeOf(tname) =>
        UncertainIntegerValue
          .above(1) // TODO: Can we use more information about sizeof?
      case UMinus(arg) => -resolve_integer_expression(arg, is_old, is_contract)
      case AmbiguousMult(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) *
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousPlus(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) +
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousMinus(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) -
          resolve_integer_expression(right, is_old, is_contract)
      case Exp(left, right) =>
        resolve_integer_expression(left, is_old, is_contract)
          .pow(resolve_integer_expression(right, is_old, is_contract))
      case Plus(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) +
          resolve_integer_expression(right, is_old, is_contract)
      case Minus(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) -
          resolve_integer_expression(right, is_old, is_contract)
      case Mult(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) *
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousDiv(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) /
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousTruncDiv(left, right) => // TODO: Handle this?
        resolve_integer_expression(left, is_old, is_contract) /
          resolve_integer_expression(right, is_old, is_contract)
      case FloorDiv(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) /
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousMod(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) %
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousTruncMod(left, right) => // TODO: Handle this?
        resolve_integer_expression(left, is_old, is_contract) %
          resolve_integer_expression(right, is_old, is_contract)
      case Mod(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) %
          resolve_integer_expression(right, is_old, is_contract)
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
          value = value
            .union(resolve_integer_expression(ift, is_old, is_contract))
            .asInstanceOf[UncertainIntegerValue]
        }
        if (
          resolve_boolean_expression(cond, is_old, is_contract).can_be_false
        ) {
          value = value
            .union(resolve_integer_expression(iff, is_old, is_contract))
            .asInstanceOf[UncertainIntegerValue]
        }
        value
      case Old(exp, at) =>
        at match {
          case Some(_) =>
            throw new IllegalArgumentException(
              s"Cannot resolve labelled old expression ${expr.toInlineString}"
            )
          case None =>
            resolve_integer_expression(exp, is_old = true, is_contract)
        }
      case l: Local[_] =>
        local.get(get_local_var(l)) match {
          case Some(value) => value.asInstanceOf[UncertainIntegerValue]
          case None => UncertainIntegerValue.uncertain()
        }
      case DerefHeapVariable(_) | Deref(_, _) | DerefPointer(_) |
          AmbiguousSubscript(_, _) | SeqSubscript(_, _) | ArraySubscript(_, _) |
          PointerSubscript(_, _) =>
        variable_from_expr(expr) match {
          case Some(v) =>
            if (is_contract && !is_old)
              UncertainIntegerValue.uncertain()
            else
              valuations(v).asInstanceOf[UncertainIntegerValue]
          case None =>
            parameter_from_expr(expr) match {
              case Some(v) => parameters(v).asInstanceOf[UncertainIntegerValue]
              case None => UncertainIntegerValue.uncertain()
            }
        }
      case Length(arr) =>
        variable_from_expr(expr) match {
          case Some(v) =>
            if (is_contract && !is_old)
              UncertainIntegerValue.above(0)
            else
              valuations(v).asInstanceOf[UncertainIntegerValue]
          case None => resolve_collection_expression(arr).len
        }
      case Size(obj) =>
        variable_from_expr(expr) match {
          case Some(v) =>
            if (is_contract && !is_old)
              UncertainIntegerValue.above(0)
            else
              valuations(v).asInstanceOf[UncertainIntegerValue]
          case None => resolve_collection_expression(obj).len
        }
      case ProcedureInvocation(ref, args, _, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainIntegerValue]
      case MethodInvocation(_, ref, args, _, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainIntegerValue]
      case FunctionInvocation(ref, args, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainIntegerValue]
      case InstanceFunctionInvocation(_, ref, args, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainIntegerValue]
      case Result(_) => UncertainIntegerValue.uncertain()
      case AmbiguousResult() => UncertainIntegerValue.uncertain()
      case InlinePattern(body, _, _) => resolve_integer_expression(body)
    }

  /** Evaluates a boolean expression and returns an uncertain boolean value.
    *
    * @param expr
    *   boolean-type COL expression
    * @return
    *   An uncertain boolean value that represents all possible values that the
    *   given expression can take on
    */
  def resolve_boolean_expression(
      expr: Expr[G],
      is_old: Boolean = false,
      is_contract: Boolean = false,
  )(implicit context: Expr[G] = expr): UncertainBooleanValue =
    expr match {
      case BooleanValue(value) => UncertainBooleanValue.from(value)
      case Not(arg) => !resolve_boolean_expression(arg)
      case AmbiguousOr(left, right) =>
        resolve_boolean_expression(left, is_old, is_contract) ||
        resolve_boolean_expression(right, is_old, is_contract)
      case Star(left, right) =>
        resolve_boolean_expression(left, is_old, is_contract) &&
        resolve_boolean_expression(right, is_old, is_contract)
      case And(left, right) =>
        resolve_boolean_expression(left, is_old, is_contract) &&
        resolve_boolean_expression(right, is_old, is_contract)
      case Or(left, right) =>
        resolve_boolean_expression(left, is_old, is_contract) ||
        resolve_boolean_expression(right, is_old, is_contract)
      case Implies(left, right) =>
        (!resolve_boolean_expression(left, is_old, is_contract)) ||
        resolve_boolean_expression(right, is_old, is_contract)
      case AmbiguousEq(left, right, _) =>
        handle_equality(left, right, is_old, is_contract, negate = false)
      case Eq(left, right) =>
        handle_equality(left, right, is_old, is_contract, negate = false)
      case AmbiguousNeq(left, right, _) =>
        handle_equality(left, right, is_old, is_contract, negate = true)
      case Neq(left, right) =>
        handle_equality(left, right, is_old, is_contract, negate = true)
      case AmbiguousGreater(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) >
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousLess(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) <
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousGreaterEq(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) >=
          resolve_integer_expression(right, is_old, is_contract)
      case AmbiguousLessEq(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) <=
          resolve_integer_expression(right, is_old, is_contract)
      case Greater(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) >
          resolve_integer_expression(right, is_old, is_contract)
      case Less(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) <
          resolve_integer_expression(right, is_old, is_contract)
      case GreaterEq(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) >=
          resolve_integer_expression(right, is_old, is_contract)
      case LessEq(left, right) =>
        resolve_integer_expression(left, is_old, is_contract) <=
          resolve_integer_expression(right, is_old, is_contract)
      case Select(cond, ift, iff) =>
        var value: UncertainBooleanValue = UncertainBooleanValue.empty()
        if (resolve_boolean_expression(cond, is_old, is_contract).can_be_true) {
          value = value
            .union(resolve_boolean_expression(ift, is_old, is_contract))
            .asInstanceOf[UncertainBooleanValue]
        }
        if (
          resolve_boolean_expression(cond, is_old, is_contract).can_be_false
        ) {
          value = value
            .union(resolve_boolean_expression(iff, is_old, is_contract))
            .asInstanceOf[UncertainBooleanValue]
        }
        value
      case Old(exp, at) =>
        at match {
          case Some(_) =>
            throw new IllegalArgumentException(
              s"Cannot resolve labelled old expression ${expr.toInlineString}"
            )
          case None =>
            resolve_boolean_expression(exp, is_old = true, is_contract)
        }
      case l: Local[_] =>
        local.get(get_local_var(l)) match {
          case Some(value) => value.asInstanceOf[UncertainBooleanValue]
          case None => UncertainBooleanValue.uncertain()
        }
      case DerefHeapVariable(_) | Deref(_, _) | DerefPointer(_) |
          AmbiguousSubscript(_, _) | SeqSubscript(_, _) | ArraySubscript(_, _) |
          PointerSubscript(_, _) =>
        variable_from_expr(expr) match {
          case Some(v) =>
            if (is_contract && !is_old)
              UncertainBooleanValue.uncertain()
            else
              valuations(v).asInstanceOf[UncertainBooleanValue]
          case None =>
            parameter_from_expr(expr) match {
              case Some(v) => parameters(v).asInstanceOf[UncertainBooleanValue]
              case None => UncertainBooleanValue.uncertain()
            }
        }
      case ProcedureInvocation(ref, args, _, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainBooleanValue]
      case MethodInvocation(_, ref, args, _, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainBooleanValue]
      case FunctionInvocation(ref, args, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainBooleanValue]
      case InstanceFunctionInvocation(_, ref, args, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainBooleanValue]
      case Held(_) =>
        UncertainBooleanValue
          .from(
            lock.nonEmpty
          ) // TODO: This means that ANY process holds the lock!
      case Scale(_, res) =>
        resolve_boolean_expression(
          res,
          is_old,
          is_contract,
        ) // TODO: Do anything with permission fraction?
      case q: Binder[_] =>
        resolve_boolean_expression(unroll_quantifier(q, context))
      case Result(_) => UncertainBooleanValue.uncertain()
      case AmbiguousResult() => UncertainBooleanValue.uncertain()
      case SeqMember(x, xs) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .contains(resolve_single_expression(x, is_old, is_contract))
      case AmbiguousMember(x, xs) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .contains(resolve_single_expression(x, is_old, is_contract))
      case InlinePattern(body, _, _) => resolve_boolean_expression(body)
      case PredicateApplyExpr(apply) => resolve_predicate_apply(apply)
      // TODO: Should these be evaluated in some way?
      case IdleToken(_) => UncertainBooleanValue.from(true)
      case Perm(_, _) => UncertainBooleanValue.from(true)
      case Committed(_) => UncertainBooleanValue.from(true)
    }

  /** Evaluates a collection expression and returns an uncertain collection
    * value.
    *
    * @param expr
    *   collection-type COL expression
    * @return
    *   An uncertain collection value that represents all possible values that
    *   the given expression can take on, possibly of uncertain length and with
    *   uncertain values at uncertain indices
    */
  def resolve_collection_expression(
      expr: Expr[G],
      is_old: Boolean = false,
      is_contract: Boolean = false,
  ): UncertainSequence =
    expr match {
      // Literals
      case LiteralSeq(element, values) =>
        val elements: Seq[(UncertainIntegerValue, UncertainSingleValue)] =
          values.zipWithIndex.map(t =>
            UncertainIntegerValue.single(t._2) ->
              resolve_single_expression(t._1, is_old, is_contract)
          )
        UncertainSequence(
          UncertainIntegerValue.single(values.size),
          elements,
          elements.map(t => t._1)
            .fold(UncertainSingleValue.uncertain_of(element))((v1, v2) =>
              v1.union(v2).asInstanceOf[UncertainSingleValue]
            ),
          element,
        )
      case UntypedLiteralSeq(values) =>
        val elements: Seq[(UncertainIntegerValue, UncertainSingleValue)] =
          values.zipWithIndex.map(t =>
            UncertainIntegerValue.single(t._2) ->
              resolve_single_expression(t._1, is_old, is_contract)
          )
        UncertainSequence(
          UncertainIntegerValue.single(values.size),
          elements,
          elements.map(t => t._1)
            .fold(UncertainSingleValue.uncertain_of(values.head.t))((v1, v2) =>
              v1.union(v2).asInstanceOf[UncertainSingleValue]
            ),
          values.head.t,
        )
      // Variables
      case Local(_) =>
        collection_from_variable(
          expr,
          local.asInstanceOf[Map[ConcreteVariable[G], UncertainSingleValue]],
          is_old,
          is_contract,
        )
      case Deref(_, _) =>
        collection_from_variable(
          expr,
          valuations
            .asInstanceOf[Map[ConcreteVariable[G], UncertainSingleValue]],
          is_old,
          is_contract,
        )
      // Array operations
      case Values(arr, from, to) =>
        resolve_collection_expression(arr, is_old, is_contract).slice(
          resolve_integer_expression(from, is_old, is_contract),
          resolve_integer_expression(to, is_old, is_contract),
        )
      // TODO: Implement array semantics
      case NewArray(element, dims, moreDims, initialize) =>
        UncertainSequence.uncertain(element)
      // Sequence operations
      case Cons(x, xs) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .prepend(resolve_single_expression(x, is_old, is_contract))
      case AmbiguousPlus(xs, ys) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .concat(resolve_collection_expression(ys, is_old, is_contract))
      case Concat(xs, ys) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .concat(resolve_collection_expression(ys, is_old, is_contract))
      case Drop(xs, count) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .drop(resolve_integer_expression(count, is_old, is_contract))
      case Take(xs, count) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .take(resolve_integer_expression(count, is_old, is_contract))
      case SeqUpdate(xs, i, x) =>
        resolve_collection_expression(xs, is_old, is_contract).updated(
          resolve_integer_expression(i, is_old, is_contract),
          resolve_single_expression(x, is_old, is_contract),
        )
      case RemoveAt(xs, i) =>
        resolve_collection_expression(xs, is_old, is_contract)
          .remove(resolve_integer_expression(i, is_old, is_contract))
      case Slice(xs, from, to) =>
        resolve_collection_expression(xs, is_old, is_contract).slice(
          resolve_integer_expression(from, is_old, is_contract),
          resolve_integer_expression(to, is_old, is_contract),
        )
      // Other expressions that can evaluate to a collection
      case Select(cond, ift, iff) =>
        val condition: UncertainBooleanValue = resolve_boolean_expression(
          cond,
          is_old,
          is_contract,
        )
        val ift_seq: UncertainSequence = resolve_collection_expression(
          ift,
          is_old,
          is_contract,
        )
        val iff_seq: UncertainSequence = resolve_collection_expression(
          iff,
          is_old,
          is_contract,
        )
        if (condition.can_be_true && condition.can_be_false)
          ift_seq.union(iff_seq)
        else if (condition.can_be_true)
          ift_seq
        else if (condition.can_be_false)
          iff_seq
        else
          UncertainSequence.empty(ift_seq.typ)
      case Old(expr, _) =>
        resolve_collection_expression(expr, is_old = true, is_contract)
      case ProcedureInvocation(ref, args, _, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainSequence]
      case MethodInvocation(_, ref, args, _, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainSequence]
      case FunctionInvocation(ref, args, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainSequence]
      case InstanceFunctionInvocation(_, ref, args, _, _, _) =>
        get_subroutine_return(
          ref.decl.contract.ensures,
          Map.from(ref.decl.args.zip(args)),
          ref.decl.returnType,
        ).asInstanceOf[UncertainSequence]
      case Result(
            applicable
          ) => // TODO: Calculate this during the calculation of a contract
        UncertainSequence.uncertain(
          applicable.decl.returnType.asInstanceOf[CompositeType[G]]
            .composingTypes.head
        )
    }

  private def collection_from_variable(
      expr: Expr[G],
      variables_to_check: Map[ConcreteVariable[G], UncertainSingleValue],
      is_old: Boolean,
      is_contract: Boolean,
  ): UncertainSequence = {
    val affected: Set[FieldIndexedVariable[G]] = variables_to_check.keySet
      .filter(v => v.is_contained_by(expr, this)).collect {
        case v: FieldIndexedVariable[_] => v
      }
    val size_var: Option[FieldSizeVariable[G]] = variables_to_check.keySet
      .filter(v => v.is_contained_by(expr, this)).collectFirst {
        case v: FieldSizeVariable[_] => v
      }
    val len: Option[UncertainIntegerValue] = size_var
      .map(v => variables_to_check(v).asInstanceOf[UncertainIntegerValue])
    val t: Type[G] =
      expr.t match {
        case TArray(element) => element
        case TSeq(element) => element
        case _ =>
          throw new IllegalArgumentException(
            s"Unsupported collection type ${expr.t.toInlineString}"
          )
      }
    if (is_contract && !is_old)
      UncertainSequence.uncertain(t)
    else
      UncertainSequence(
        len.getOrElse(UncertainIntegerValue.above(
          if (affected.isEmpty)
            0
          else
            affected.map(v => v.i).max
        )),
        affected
          .map(v => UncertainIntegerValue.single(v.i) -> variables_to_check(v))
          .toSeq,
        UncertainSingleValue.uncertain_of(t),
        t,
      )
  }

  private def get_subroutine_return(
      post: AccountedPredicate[G],
      args: Map[Variable[G], Expr[G]],
      return_type: Type[G],
  ): UncertainValue =
    get_return(
      Utils.unify_expression(Utils.contract_to_expression(post), args),
      return_type,
    )

  private def get_return(
      contract: Expr[G],
      return_type: Type[G],
  ): UncertainValue = {
    val result_var: ResultSimpleVariable[G] = ResultSimpleVariable(return_type)
    val result_set: Set[ResolvableVariable[G]] = Set(result_var)
    val constraints: Set[ConstraintMap[G]] =
      new ConstraintSolver(this, result_set, true).resolve_assumption(contract)
        .filter(m => !m.is_impossible)
    val possible_vals: Set[UncertainValue] = constraints.map(m =>
      m.resolve.getOrElse(result_var, UncertainValue.uncertain_of(return_type))
    )
    possible_vals
      .reduce((v1, v2) => v1.union(v2).asInstanceOf[UncertainSingleValue])
  }

  private def handle_equality(
      left: Expr[G],
      right: Expr[G],
      is_old: Boolean,
      is_contract: Boolean,
      negate: Boolean,
  ): UncertainBooleanValue =
    left.t match {
      case _: IntType[_] | TBool() =>
        val left_val: UncertainSingleValue = resolve_single_expression(
          left,
          is_old,
          is_contract,
        )
        val right_val: UncertainSingleValue = resolve_single_expression(
          right,
          is_old,
          is_contract,
        )
        if (!negate)
          left_val == right_val
        else
          left_val != right_val
      case _: TSeq[_] | TArray(_) =>
        val left_coll: UncertainSequence = resolve_collection_expression(
          left,
          is_old,
          is_contract,
        )
        val right_coll: UncertainSequence = resolve_collection_expression(
          right,
          is_old,
          is_contract,
        )
        val equals: UncertainBooleanValue = left_coll == right_coll
        if (!negate)
          equals
        else
          !equals
      case _ => UncertainBooleanValue.from(true)
      // TODO: The justification for this is that the program will be verified anyway,
      //  so object equality does not need to be considered; does that make sense?
    }

  private def variable_from_expr(variable: Expr[G]): Option[FieldVariable[G]] =
    valuations.keys.collectFirst {
      case c: FieldVariable[G] if c.is(variable, this) => c
    }

  private def parameter_from_expr(
      variable: Expr[G]
  ): Option[FieldSimpleVariable[G]] =
    parameters.keys.collectFirst {
      case f: FieldSimpleVariable[G] if f.is(variable, this) => f
    }

  private def get_local_var(variable: Local[G]): LocalSimpleVariable[G] =
    local.keys.collectFirst {
      case l: LocalSimpleVariable[G] if l.is(variable, this) => l
    } match {
      case Some(v) => v
      case None => LocalSimpleVariable(variable.ref.decl)
    }

  private def resolve_predicate_apply(
      pred: ApplyAnyPredicate[G]
  ): UncertainBooleanValue = {
    val (body: Expr[G], params: Seq[Variable[G]], vals: Seq[Expr[G]]) =
      pred match {
        case PredicateApply(ref, args) =>
          (
            ref.decl.body.getOrElse(return UncertainBooleanValue.from(true)),
            ref.decl.args,
            args,
          )
        case InstancePredicateApply(_, ref, args) =>
          (
            ref.decl.body.getOrElse(return UncertainBooleanValue.from(true)),
            ref.decl.args,
            args,
          )
      }
    resolve_boolean_expression(
      Utils.unify_expression(body, Map.from(params.zip(vals)))
    )
  }

  private def unroll_quantifier(q: Binder[G], context: Expr[G]): Expr[G] =
    q match {
      case Forall(bindings, _, body) =>
        resolve_forall(
          bindings,
          body,
          (e1: Expr[G], e2: Expr[G]) => And(e1, e2)(q.o),
          context,
        )
      case Starall(bindings, _, body) =>
        resolve_forall(
          bindings,
          body,
          (e1: Expr[G], e2: Expr[G]) => Star(e1, e2)(q.o),
          context,
        )
      case Exists(bindings, _, body) => resolve_exists(bindings, body, context)
    }

  private def resolve_forall(
      iterators: Seq[Variable[G]],
      body: Expr[G],
      operator: (Expr[G], Expr[G]) => Expr[G],
      context: Expr[G],
  ): Expr[G] =
    body match {
      case Implies(left, _) =>
        left match {
          case a: And[G] =>
            assemble_quantifier(
              iterators,
              a,
              context,
              bounds =>
                Utils.replace_iterators_in_quantifier(
                  bounds,
                  body,
                  operator,
                  BooleanValue(value = true)(body.o),
                ),
            )
          case _ =>
            throw new IllegalArgumentException(
              "Unsupported universal quantifier format " + body.toInlineString
            )
        }
      case _ =>
        throw new IllegalArgumentException(
          "Unsupported universal quantifier format " + body.toInlineString
        )
    }

  private def resolve_exists(
      iterators: Seq[Variable[G]],
      body: Expr[G],
      context: Expr[G],
  ): Expr[G] =
    body match {
      case a: And[G] =>
        assemble_quantifier(
          iterators,
          a,
          context,
          bounds =>
            Utils.replace_iterators_in_quantifier(
              bounds,
              body,
              (e1: Expr[G], e2: Expr[G]) => Or(e1, e2)(body.o),
              BooleanValue(value = false)(body.o),
            ),
        )
      case _ =>
        throw new IllegalArgumentException(
          "Unsupported existential quantifier format " + body.toInlineString
        )
    }

  private def assemble_quantifier(
      iterators: Seq[Variable[G]],
      bound_condition: And[G],
      context: Expr[G],
      assemble: Map[Variable[G], (Int, Int)] => Expr[G],
  ): Expr[G] = {
    val bounds_expressions: Map[Variable[G], ((Expr[G], Int), (Expr[G], Int))] =
      Map.from(iterators.map(v => v -> get_iterator_bounds(v, bound_condition)))
    val bounds_ranges: Map[Variable[G], ((Int, Int), (Int, Int))] =
      bounds_expressions.map(t =>
        t._1 ->
          (
            resolve_bound_expression(t._2._1._1, t._2._1._2, context),
            resolve_bound_expression(t._2._2._1, t._2._2._2, context),
          )
      )
    val (
      certain_ranges: Map[Variable[G], ((Int, Int), (Int, Int))],
      uncertain_ranges: Map[Variable[G], ((Int, Int), (Int, Int))],
    ) = bounds_ranges
      .partition(t => t._2._1._1 == t._2._1._2 && t._2._2._1 == t._2._2._2)
    val certain: Map[Variable[G], (Int, Int)] = certain_ranges
      .map(t => t._1 -> (t._2._1._1, t._2._2._1))

    if (uncertain_ranges.isEmpty)
      assemble(certain)
    else {
      val uncertain: Map[Variable[G], (Seq[(Int, Int)], (Boolean, Boolean))] =
        uncertain_ranges.map(t =>
          t._1 ->
            (
              for {
                x <- t._2._1._1 to t._2._1._2; y <- t._2._2._1 to t._2._2._2
              } yield (x, y),
              (t._2._1._1 != t._2._1._2, t._2._2._1 != t._2._2._2),
            )
        )
      val certain_set: Seq[Map[Variable[G], ((Int, Int), (Boolean, Boolean))]] =
        Utils
          .extract_uncertainty[(Variable[G], (Boolean, Boolean)), (Int, Int)](
            uncertain.map(t => (t._1, t._2._2) -> t._2._1)
          ).map(m => m.map(t => t._1._1 -> (t._2, t._1._2)))
      val implications: Seq[Expr[G]] = certain_set.map(m =>
        get_quantifier_implication(
          certain,
          m,
          bounds_expressions.map(t =>
            t._1 -> ((t._2._1._1, t._2._2._1), (t._2._1._2, t._2._2._2))
          ),
          assemble,
        )
      )
      Utils.fold_and(implications)
    }
  }

  private def get_quantifier_implication(
      certain_vars: Map[Variable[G], (Int, Int)],
      uncertain_vars: Map[Variable[G], ((Int, Int), (Boolean, Boolean))],
      expr_lookup: Map[Variable[G], ((Expr[G], Expr[G]), (Int, Int))],
      assemble: Map[Variable[G], (Int, Int)] => Expr[G],
  ): Implies[G] = {
    val conds: Seq[Expr[G]] =
      uncertain_vars.map(t =>
        get_instance_condition(
          t._2._1,
          t._2._2,
          expr_lookup(t._1)._1,
          expr_lookup(t._1)._2,
        )
      ).toSeq
    val quantifier_instance: Expr[G] = assemble(
      certain_vars ++ uncertain_vars.map(t => t._1 -> t._2._1)
    )
    Implies(Utils.fold_and(conds), quantifier_instance)(Utils.origen)
  }

  private def get_instance_condition(
      bounds: (Int, Int),
      uncertain: (Boolean, Boolean),
      exprs: (Expr[G], Expr[G]),
      offsets: (Int, Int),
  ): Expr[G] = {
    val lower_condition: Eq[G] =
      Eq(exprs._1, IntegerValue(bounds._1 - offsets._1)(Utils.origen))(
        Utils.origen
      )
    val upper_condition: Eq[G] =
      Eq(exprs._2, IntegerValue(bounds._2 - offsets._2)(Utils.origen))(
        Utils.origen
      )
    var conds: Seq[Expr[G]] = Seq()
    if (uncertain._1)
      conds = conds :+ lower_condition
    if (uncertain._2)
      conds = conds :+ upper_condition
    Utils.fold_and(conds)
  }

  private def get_iterator_bounds(
      iterator: Variable[G],
      cond: And[G],
  ): ((Expr[G], Int), (Expr[G], Int)) = {
    def iterator_is_left(e1: Expr[G], e2: Expr[G]): Option[Boolean] =
      e1 match {
        case Local(ref) if ref.decl == iterator => Some(true)
        case _ =>
          e2 match {
            case Local(ref) if ref.decl == iterator => Some(false)
            case _ => None
          }
      }
    val conds: Seq[Expr[G]] = Utils.split_conjunction(cond)
    (
      iterator_bound(iterator_is_left, conds, lower = true),
      iterator_bound(iterator_is_left, conds, lower = false),
    )
  }

  private def iterator_bound(
      iterator_is_left: (Expr[G], Expr[G]) => Option[Boolean],
      conds: Seq[Expr[G]],
      lower: Boolean,
  ): (Expr[G], Int) =
    conds.collectFirst {
      case Less(left, right)
          if iterator_is_left(left, right).contains(!lower) =>
        if (lower)
          (left, 1)
        else
          (right, -1)
      case LessEq(left, right)
          if iterator_is_left(left, right).contains(!lower) =>
        if (lower)
          (left, 0)
        else
          (right, 0)
      case Greater(left, right)
          if iterator_is_left(left, right).contains(lower) =>
        if (lower)
          (right, -1)
        else
          (left, 1)
      case GreaterEq(left, right)
          if iterator_is_left(left, right).contains(lower) =>
        if (lower)
          (right, 0)
        else
          (left, 0)
      case AmbiguousLess(left, right)
          if iterator_is_left(left, right).contains(!lower) =>
        if (lower)
          (left, 1)
        else
          (right, -1)
      case AmbiguousLessEq(left, right)
          if iterator_is_left(left, right).contains(!lower) =>
        if (lower)
          (left, 0)
        else
          (right, 0)
      case AmbiguousGreater(left, right)
          if iterator_is_left(left, right).contains(lower) =>
        if (lower)
          (right, -1)
        else
          (left, 1)
      case AmbiguousGreaterEq(left, right)
          if iterator_is_left(left, right).contains(lower) =>
        if (lower)
          (right, 0)
        else
          (left, 0)
    }.getOrElse(
      throw new IllegalStateException(
        "Malformed quantifier: Quantifier must declare iterator bounds!"
      )
    )

  private def resolve_bound_expression(
      e: Expr[G],
      offset: Int,
      context: Expr[G],
  ): (Int, Int) = {
    val resolved: Option[Int] = try_to_get_bound(e)
    if (resolved.nonEmpty)
      (resolved.get + offset, resolved.get + offset)
    else {
      def left_is_equal_to_expression(
          e1: Expr[G],
          e2: Expr[G],
      ): Option[Boolean] =
        if (e1.equals(e))
          Some(true)
        else if (e2.equals(e))
          Some(false)
        else
          None
      val conds: Seq[Expr[G]] = Utils.split_conjunction(context)
      val (min_expr: Expr[G], min_offset: Int) = iterator_bound(
        left_is_equal_to_expression,
        conds,
        lower = true,
      )
      val (max_expr: Expr[G], max_offset: Int) = iterator_bound(
        left_is_equal_to_expression,
        conds,
        lower = false,
      )
      (
        resolve_integer_expression(min_expr).try_to_resolve().getOrElse(
          throw new IllegalStateException(
            "Could not find a lower context bound for " + e.toInlineString
          )
        ) + min_offset,
        resolve_integer_expression(max_expr).try_to_resolve().getOrElse(
          throw new IllegalStateException(
            "Could not find an upper context bound for " + e.toInlineString
          )
        ) + max_offset,
      )
    }
  }

  private def try_to_get_bound(e: Expr[G]): Option[Int] =
    e match {
      case Size(obj) => resolve_collection_expression(obj).len.min()
      case _ => resolve_integer_expression(e).try_to_resolve()
    }

  /** Returns an expression to represent this state of the form <code>variable1
    * \== value1 && variable2 == value2 && ...</code>
    *
    * @return
    *   An expression that encodes this state
    */
  def to_expression(objs: Option[Map[FieldVariable[G], Expr[G]]]): Expr[G] = {
    val sorted_valuations = valuations.toSeq
      .sortWith((t1, t2) => t1._1.compare(t2._1))
    sorted_valuations.map(v =>
      v._2.to_expression(
        v._1.to_expression(Option.when(objs.nonEmpty)(objs.get.apply(v._1)))
      )
    ).reduce((e1, e2) => And(e1, e2)(e1.o))
  }
}
