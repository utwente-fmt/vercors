package vct.rewrite.rasi

case class RASIEdge[G](
    from: AbstractState[G],
    vars: Set[ConcreteVariable[G]],
    to: AbstractState[G],
)

sealed trait RASISuccessor[G] {
  def is_empty: Boolean
  def successors: Set[AbstractState[G]]
  def distinguish_by: Set[ConcreteVariable[G]]
  def update_each(f: AbstractState[G] => RASISuccessor[G]): RASISuccessor[G]
  def edges(start: AbstractState[G]): Set[RASIEdge[G]]
  def removed_states(states: Set[AbstractState[G]]): RASISuccessor[G]
  def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G]
}

case object RASISuccessor {
  private def from[G](
      variables: Set[ConcreteVariable[G]],
      states: Set[AbstractState[G]],
  ): RASISuccessor[G] =
    if (states.isEmpty)
      AlternativeSuccessor(Set())
    else if (states.size == 1)
      SingleSuccessor(states.head)
    else if (variables.isEmpty)
      AlternativeSuccessor(states.map(s => SingleSuccessor(s)))
    else
      DistinguishedSuccessor(variables, states.map(s => SingleSuccessor(s)))

  def apply[G](
      variables: Set[ConcreteVariable[G]],
      states: Set[AbstractState[G]],
  ): RASISuccessor[G] = from(variables, states)

  private def combine[G](
      successor_set: Set[RASISuccessor[G]]
  ): RASISuccessor[G] = {
    AlternativeSuccessor(successor_set.map(r =>
      r.factor_out(successor_set.diff(Set(r)).flatMap(s => s.successors))
    ))
  }

  def apply[G](successors: Set[RASISuccessor[G]]): RASISuccessor[G] =
    combine(successors)
}

case class AlternativeSuccessor[G](successor_set: Set[RASISuccessor[G]])
    extends RASISuccessor[G] {
  override def is_empty: Boolean = successor_set.forall(r => r.is_empty)

  override def successors: Set[AbstractState[G]] =
    successor_set.flatMap(r => r.successors)

  override def distinguish_by: Set[ConcreteVariable[G]] =
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
    if (filtered.size == 1)
      filtered.head
    else
      AlternativeSuccessor(filtered)
  }

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] =
    AlternativeSuccessor(successor_set.map(r => r.factor_out(states)))
}

case class SingleSuccessor[G](successor: AbstractState[G])
    extends RASISuccessor[G] {
  override def is_empty: Boolean = false

  override def successors: Set[AbstractState[G]] = Set(successor)

  override def distinguish_by: Set[ConcreteVariable[G]] = Set()

  override def update_each(
      f: AbstractState[G] => RASISuccessor[G]
  ): RASISuccessor[G] = f(successor)

  override def edges(start: AbstractState[G]): Set[RASIEdge[G]] =
    Set(RASIEdge(start, Set(), successor))

  override def removed_states(states: Set[AbstractState[G]]): RASISuccessor[G] =
    if (states.contains(successor))
      AlternativeSuccessor(Set())
    else
      this

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] =
    this
}

case class DistinguishedSuccessor[G](
    distinguishing_variables: Set[ConcreteVariable[G]],
    successor_set: Set[RASISuccessor[G]],
) extends RASISuccessor[G] {
  override def is_empty: Boolean = successor_set.forall(r => r.is_empty)

  override def successors: Set[AbstractState[G]] =
    successor_set.flatMap(r => r.successors)

  override def distinguish_by: Set[ConcreteVariable[G]] =
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
      AlternativeSuccessor(Set())
    else if (filtered.size == 1)
      filtered.head
    else
      DistinguishedSuccessor(distinguishing_variables, filtered)
  }

  override def factor_out(states: Set[AbstractState[G]]): RASISuccessor[G] = {
    val hit: Set[AbstractState[G]] = states.intersect(successors)
    if (hit.nonEmpty)
      AlternativeSuccessor(
        hit.map[RASISuccessor[G]](s => SingleSuccessor(s)) + this.removed_states(hit)
      )
    else
      DistinguishedSuccessor(
        distinguishing_variables,
        successor_set.map(r => r.factor_out(states)),
      )
  }
}
