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
    val constraints: Seq[Set[ConstraintMap[G]]] = states.map(s => satisfying_valuations(s, expr)).filter(s => s.exists(m => !m.is_empty))
    if (constraints.isEmpty) Set.empty[ConcreteVariable[G]]
    else var_differences(constraints)
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

    val constraints: Seq[Set[ConstraintMap[G]]] = conditions.toSeq.map(e => satisfying_valuations(initial_state, e)).filter(s => s.exists(m => !m.is_empty))
    if (constraints.isEmpty) Set.empty[ConcreteVariable[G]]
    else var_differences(constraints)
  }

  private def satisfying_valuations(state: AbstractState[G], expr: Option[Expr[G]]): Set[ConstraintMap[G]] = expr match {
    case None => Set(ConstraintMap.empty[G])
    case Some(e) =>
      if (!state.resolve_boolean_expression(e).can_be_false) Set(ConstraintMap.empty[G])
      else {
        val variables: Set[ResolvableVariable[G]] = free_variables(state, e)
        new ConstraintSolver(state, variables, is_contract = false).resolve_assumption(e).filter(m => !m.is_impossible)
      }
  }

  private def free_variables(state: AbstractState[G], expr: Expr[G]): Set[ResolvableVariable[G]] = expr match {
    case d @ Deref(_, ref) => if (state.valuations.exists(t => t._1.is_contained_by(d, state))) Set() else Set(FieldVariable(ref.decl))
    case l @ Local(ref) => if (state.valuations.exists(t => t._1.is(l, state))) Set() else Set(LocalVariable(ref.decl))
    case a @ AmbiguousSubscript(collection, index) => create_indexed_var_if_needed(state, a, collection, index)
    case s @ SeqSubscript(seq, index) => create_indexed_var_if_needed(state, s, seq, index)
    case a @ ArraySubscript(arr, index) => create_indexed_var_if_needed(state, a, arr, index)
    case s @ Size(obj) =>
      if (state.valuations.exists(t => t._1.is(s, state))) Set()
      else obj match {
        case Deref(_, ref) => Set(SizeVariable(ref.decl))
      }
    case _ => expr.subnodes.toSet[Node[G]].collect[Expr[G]]{ case e: Expr[_] => e }.flatMap(e => free_variables(state, e))
  }

  private def create_indexed_var_if_needed(state: AbstractState[G], expr: Expr[G], collection: Expr[G], subscript: Expr[G]): Set[ResolvableVariable[G]] = {
    if (state.valuations.exists(t => t._1.is(expr, state))) Set()
    else {
      val field: InstanceField[G] = collection match {
        case Deref(_, ref) => ref.decl
        case _ => throw new IllegalStateException("Collection value is not dereferenced instance field!")
      }
      val index: Option[Int] = state.resolve_integer_expression(subscript).try_to_resolve()
      if (index.isEmpty) Set()
      else Set(IndexedVariable(field, index.get))
    }
  }

  private def var_differences(constraints: Seq[Set[ConstraintMap[G]]]): Set[ConcreteVariable[G]] = {
    // TODO: This causes some overapproximation, since constraints between the different variables are broken up. These
    //       should probably be handled as sets of constraint maps, but that makes the meaning much more difficult to
    //       keep track of
    val c: Seq[Map[ConcreteVariable[G], UncertainValue]] = constraints.map(s => Utils.resolvable_to_concrete(s.reduce((m1, m2) => m1 || m2).resolve))
    val vars: Seq[ConcreteVariable[G]] = c.flatMap(m => m.keySet)
    var s: Set[ConcreteVariable[G]] = Set()
    for (v <- vars) {
      val t: Seq[UncertainValue] = c.filter(m => m.contains(v)).map(m => m(v))
      if (t.exists(_ != t.head)) s += v
    }
    s
  }
}
