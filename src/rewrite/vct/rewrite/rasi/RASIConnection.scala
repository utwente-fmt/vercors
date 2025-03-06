package vct.rewrite.rasi

case class RASIEdge[G](
    from: AbstractState[G],
    vars: Set[FieldVariable[G]],
    to: AbstractState[G],
)

/** Describes a state's possible successors in the state space. It can contain
  * zero, one or multiple abstract states as well as variables that can
  * distinguish between them.
  */
sealed trait RASISuccessor[G] {

  /** Indicates whether this <code>RASISuccessor</code> is empty or whether it
    * contains a successor state.
    *
    * @return
    *   <code>true</code> if it is empty, <code>false</code> otherwise
    */
  def is_empty: Boolean

  /** Returns all states contained in this successor object.
    *
    * @return
    *   A set of all successor states
    */
  def successors: Set[AbstractState[G]]

  /** Returns the set of variables that can distinguish between the successor
    * states stored here.
    *
    * @return
    *   A set of distinguishing untracked variables
    */
  def distinguish_by: Set[FieldVariable[G]]

  /** Updates all contained abstract successor states according to
    * <code>f</code>, but leaves any potential distinguishing variables intact.
    *
    * @param f
    *   Update function that takes an abstract state and transforms it to a
    *   <code>RASISuccessor</code>
    * @return
    *   The result of applying <code>f</code> to all successor states
    */
  def update_each(f: AbstractState[G] => RASISuccessor[G]): RASISuccessor[G]

  /** Given a starting state, generates the execution graph edges to all
    * successor states.
    *
    * @param start
    *   Starting state
    * @return
    *   A set of edges from <code>start</code> to its successors
    */
  def edges(start: AbstractState[G]): Set[RASIEdge[G]]

  /** Removes a set of states from this successor object.
    *
    * @param states
    *   Set of states to remove
    * @return
    *   A successor with the given states removed (possibly of a different
    *   subtype, possibly empty)
    */
  def removed_states(states: Set[AbstractState[G]]): RASISuccessor[G]

  /** Factors out the given states, not removing them, but not considering them
    * for distinguishing variables, either.
    *
    * @param states
    *   Set of states to factor out for variable determination
    * @return
    *   A successor state with the same
    */
  def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G]
}
case object RASISuccessor {
  def apply[G](
      variables: Set[FieldVariable[G]],
      states: Set[AbstractState[G]],
  ): RASISuccessor[G] = from(variables, states)

  def apply[G](
      successors: Map[AbstractProcess[G], RASISuccessor[G]]
  ): RASISuccessor[G] = combine(successors)

  private def from[G](
      variables: Set[FieldVariable[G]],
      states: Set[AbstractState[G]],
  ): RASISuccessor[G] =
    if (states.isEmpty)
      EmptySuccessor()
    else if (states.size == 1)
      SingleSuccessor(states.head)
    else if (variables.isEmpty)
      AlternativeSuccessor(states.map(s => SingleSuccessor(s)))
    else
      DistinguishedSuccessor(variables, states.map(s => SingleSuccessor(s)))

  private def combine[G](
      successor_set: Map[AbstractProcess[G], RASISuccessor[G]]
  ): RASISuccessor[G] = {
    SimulationSuccessor(successor_set.map(t =>
      t._1 -> t._2.factor_out(
        successor_set.values.toSet.diff(Set(t._2)).flatMap(s => s.successors)
      )
    ))
  }
}

/** Implementation for an empty successor set.
  */
case class EmptySuccessor[G]() extends RASISuccessor[G] {
  override def is_empty: Boolean = true

  override def successors: Set[AbstractState[G]] = Set()

  override def distinguish_by: Set[FieldVariable[G]] = Set()

  override def update_each(
      f: AbstractState[G] => RASISuccessor[G]
  ): RASISuccessor[G] = this

  override def edges(start: AbstractState[G]): Set[RASIEdge[G]] = Set()

  override def removed_states(states: Set[AbstractState[G]]): RASISuccessor[G] =
    this

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] =
    this
}

/** Implementation for a successor set that contains a single state.
  *
  * @param successor
  *   Singular state in the successor set
  */
case class SingleSuccessor[G](successor: AbstractState[G])
    extends RASISuccessor[G] {
  override def is_empty: Boolean = false

  override def successors: Set[AbstractState[G]] = Set(successor)

  override def distinguish_by: Set[FieldVariable[G]] = Set()

  override def update_each(
      f: AbstractState[G] => RASISuccessor[G]
  ): RASISuccessor[G] = f(successor)

  override def edges(start: AbstractState[G]): Set[RASIEdge[G]] =
    Set(RASIEdge(start, Set(), successor))

  override def removed_states(states: Set[AbstractState[G]]): RASISuccessor[G] =
    if (states.contains(successor))
      EmptySuccessor()
    else
      this

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] =
    this
}

/** Implementation for a successor set composed of multiple other successor
  * sets.
  *
  * @param successor_set
  *   Composite successor sets
  */
case class AlternativeSuccessor[G](successor_set: Set[RASISuccessor[G]])
    extends RASISuccessor[G] {
  override def is_empty: Boolean = successor_set.forall(r => r.is_empty)

  override def successors: Set[AbstractState[G]] =
    successor_set.flatMap(r => r.successors)

  override def distinguish_by: Set[FieldVariable[G]] =
    successor_set.flatMap(r => r.distinguish_by)

  override def update_each(
      f: AbstractState[G] => RASISuccessor[G]
  ): RASISuccessor[G] =
    AlternativeSuccessor(successor_set.map(r => r.update_each(f)))

  override def edges(start: AbstractState[G]): Set[RASIEdge[G]] =
    successor_set.flatMap(r => r.edges(start))

  override def removed_states(
      states: Set[AbstractState[G]]
  ): RASISuccessor[G] = {
    val filtered: Set[RASISuccessor[G]] = successor_set
      .map(r => r.removed_states(states)).filter(r => !r.is_empty)
    if (filtered.isEmpty)
      EmptySuccessor()
    else if (filtered.size == 1)
      filtered.head
    else
      AlternativeSuccessor(filtered)
  }

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] =
    AlternativeSuccessor(successor_set.map(r => r.factor_out(states)))
}

/** Implementation for the final result of a simulation step. Contains, for each
  * executable process at the time of simulation, the successor set that is
  * reachable by executing the process for one atomic step.
  *
  * @param simulation_steps
  *   Map from processes to their successor sets
  */
case class SimulationSuccessor[G](
    simulation_steps: Map[AbstractProcess[G], RASISuccessor[G]]
) extends RASISuccessor[G] {
  override def is_empty: Boolean =
    simulation_steps.values.forall(r => r.is_empty)

  override def successors: Set[AbstractState[G]] =
    simulation_steps.values.toSet.flatMap[AbstractState[G]](r => r.successors)

  override def distinguish_by: Set[FieldVariable[G]] =
    simulation_steps.values.toSet
      .flatMap[FieldVariable[G]](r => r.distinguish_by)

  override def update_each(
      f: AbstractState[G] => RASISuccessor[G]
  ): RASISuccessor[G] =
    SimulationSuccessor(simulation_steps.map(t => t._1 -> t._2.update_each(f)))

  override def edges(start: AbstractState[G]): Set[RASIEdge[G]] =
    simulation_steps.values.toSet.flatMap[RASIEdge[G]](r => r.edges(start))

  override def removed_states(states: Set[AbstractState[G]]): RASISuccessor[G] =
    SimulationSuccessor(
      simulation_steps.map(t => t._1 -> t._2.removed_states(states))
    )

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] =
    SimulationSuccessor(
      simulation_steps.map(t => t._1 -> t._2.factor_out(states))
    )
}

/** Implementation for a successor set with multiple entries that can
  * potentially be distinguished in the execution by a certain set of variables.
  *
  * @param distinguishing_variables
  *   Variables that can potentially distinguish elements of this set
  * @param successor_set
  *   Composite successor sets
  */
case class DistinguishedSuccessor[G](
    distinguishing_variables: Set[FieldVariable[G]],
    successor_set: Set[RASISuccessor[G]],
) extends RASISuccessor[G] {
  override def is_empty: Boolean = successor_set.forall(r => r.is_empty)

  override def successors: Set[AbstractState[G]] =
    successor_set.flatMap(r => r.successors)

  override def distinguish_by: Set[FieldVariable[G]] =
    distinguishing_variables ++ successor_set.flatMap(r => r.distinguish_by)

  override def update_each(
      f: AbstractState[G] => RASISuccessor[G]
  ): RASISuccessor[G] =
    DistinguishedSuccessor(
      distinguishing_variables,
      successor_set.map(r => r.update_each(f)),
    )

  override def edges(start: AbstractState[G]): Set[RASIEdge[G]] =
    successor_set.flatMap(r => r.edges(start))

  override def removed_states(
      states: Set[AbstractState[G]]
  ): RASISuccessor[G] = {
    val filtered: Set[RASISuccessor[G]] = successor_set
      .map(r => r.removed_states(states)).filter(r => !r.is_empty)
    if (filtered.isEmpty)
      EmptySuccessor()
    else if (filtered.size == 1)
      filtered.head
    else
      DistinguishedSuccessor(distinguishing_variables, filtered)
  }

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] = {
    val hit: Set[AbstractState[G]] = successors.intersect(states)
    // different result for some reason: states.intersect(successors)
    if (hit.nonEmpty)
      AlternativeSuccessor(
        hit.map[RASISuccessor[G]](s => SingleSuccessor(s)) +
          this.removed_states(hit)
      )
    else
      DistinguishedSuccessor(
        distinguishing_variables,
        successor_set.map(r => r.factor_out(states)),
      )
  }
}
