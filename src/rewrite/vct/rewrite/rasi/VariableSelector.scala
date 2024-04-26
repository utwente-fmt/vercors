package vct.rewrite.rasi

import vct.col.ast._

class VariableSelector[G](initial_state: AbstractState[G]) {

  /**
   * Finds the untracked variables that distinguish the evaluation of the given expression between the given valuations
   * of tracked variables.
   *
   * @param valuations A set of different valuations of tracked variables, each of which can potentially evaluate to
   *                   <code>true</code> on the given expression
   * @param expr Expression in which the valuations should be distinguished; <code>None</code> means <code>true</code>
   * @return A set of all variables that could, had they been known, have distinguished between at least two of the
   *         given valuations
   */
  def distinguishing_variables(valuations: Set[Map[ConcreteVariable[G], UncertainValue]], expr: Option[Expr[G]]): Set[ConcreteVariable[G]] = {
    if (valuations.size <= 1) return Set.empty[ConcreteVariable[G]]

    val states: Seq[AbstractState[G]] = valuations.toSeq.map(v => initial_state.with_new_valuation(v))
    val constraints: Seq[ConstraintMap[G]] = states.map(s => satisfying_valuations(s, expr))
    var_differences(constraints)
  }

  /**
   * Finds the untracked variables that distinguish between the given conditions in the initial state given by this
   * object.
   *
   * @param conditions A set of conditions, each of which can potentially evaluate to <code>true</code> in the initial
   *                   state
   * @return A set of all variables that could, had they been known, have distinguished between at least two of the
   *         given conditions
   */
  def deciding_variables(conditions: Set[Option[Expr[G]]]): Set[ConcreteVariable[G]] = {
    if (conditions.size <= 1) return Set.empty[ConcreteVariable[G]]

    val constraints: Seq[ConstraintMap[G]] = conditions.toSeq.map(e => satisfying_valuations(initial_state, e))
    var_differences(constraints)
  }

  private def satisfying_valuations(state: AbstractState[G], expr: Option[Expr[G]]): ConstraintMap[G] = expr match {
    case None => ConstraintMap.empty[G]
    case Some(e) =>
      if (!state.resolve_boolean_expression(e).can_be_false) ConstraintMap.empty[G]
      else find_vals(state, e, needs_to_be = true)
  }

  private def find_vals(state: AbstractState[G], expr: Expr[G], needs_to_be: Boolean): ConstraintMap[G] = {
    if (state.resolve_boolean_expression(expr).try_to_resolve().nonEmpty) return ConstraintMap.empty[G]
    expr match {
      case d @ Deref(_, ref) => handle_variable(state, d, needs_to_be).getOrElse(ConstraintMap.from(FieldVariable(ref.decl), UncertainBooleanValue.from(needs_to_be)))
      case Local(ref) => ConstraintMap.from(LocalVariable(ref.decl), UncertainBooleanValue.from(needs_to_be))
      case s @ SeqSubscript(seq, index) => handle_collection(state, s, seq, index, needs_to_be)
      case a @ ArraySubscript(arr, index) => handle_collection(state, a, arr, index, needs_to_be)
      case Not(arg) => find_vals(state, arg, !needs_to_be)
      case And(left, right) => handle_and(state, left, right, needs_to_be)
      case Star(left, right) => handle_and(state, left, right, needs_to_be)
      case AmbiguousOr(left, right) => handle_and(state, left, right, needs_to_be)
      case Or(left, right) => handle_or(state, left, right, needs_to_be)
      case Implies(left, right) =>
        if (needs_to_be) find_vals(state, left, !needs_to_be) || find_vals(state, right, needs_to_be)
        else find_vals(state, left, needs_to_be) && find_vals(state, right, !needs_to_be)
      case c: Comparison[_] => handle_comparison(state, c)
    }
  }

  private def handle_variable(state: AbstractState[G], variable: Expr[G], needs_to_be: Boolean): Option[ConstraintMap[G]] = {
    val state_entry: Option[(ConcreteVariable[G], UncertainValue)] = state.valuations.collectFirst { case (k, v) if k.is(variable, state) => (k, v) }
    if (state_entry.nonEmpty) {
      if (state_entry.get._2.can_be_equal(UncertainBooleanValue.from(needs_to_be))) Some(ConstraintMap.empty[G])
      else Some(ConstraintMap.impossible(Set(state_entry.get._1)))
    }
    else None
  }

  private def handle_collection(state: AbstractState[G], expr: Expr[G], collection: Expr[G], subscript: Expr[G], needs_to_be: Boolean): ConstraintMap[G] = {
    val state_map: Option[ConstraintMap[G]] = handle_variable(state, expr, needs_to_be)
    if (state_map.nonEmpty) state_map.get
    else {
      val field: InstanceField[G] = collection match {
        case Deref(_, ref) => ref.decl
        case _ => throw new IllegalStateException("Collection value is not dereferenced instance field!")
      }
      val index: Option[Int] = state.resolve_integer_expression(subscript).try_to_resolve()
      if (index.isEmpty) throw new IllegalStateException("Cannot resolve collection subscript!")
      ConstraintMap.from(IndexedVariable(field, index.get), UncertainBooleanValue.from(needs_to_be))
    }
  }

  private def handle_and(state: AbstractState[G], left: Expr[G], right: Expr[G], needs_to_be: Boolean): ConstraintMap[G] =
    if (needs_to_be) find_vals(state, left, needs_to_be) && find_vals(state, left, needs_to_be)
    else find_vals(state, left, !needs_to_be) || find_vals(state, right, !needs_to_be)

  private def handle_or(state: AbstractState[G], left: Expr[G], right: Expr[G], needs_to_be: Boolean): ConstraintMap[G] =
    if (needs_to_be) find_vals(state, left, needs_to_be) || find_vals(state, left, needs_to_be)
    else find_vals(state, left, !needs_to_be) && find_vals(state, right, !needs_to_be)

  private def handle_comparison(state: AbstractState[G], comp: Comparison[G]): ConstraintMap[G] = {
    val l = comp.left
    val r = comp.right
    ???
  }

  private def var_differences(constraints: Seq[ConstraintMap[G]]): Set[ConcreteVariable[G]] = {
    ???
  }
}
