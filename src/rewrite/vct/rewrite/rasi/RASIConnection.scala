package vct.rewrite.rasi

case class RASISuccessor[G](
    deciding_variables: Set[ConcreteVariable[G]],
    successors: Set[AbstractState[G]],
) {
  def edges(start: AbstractState[G]): Set[RASIEdge[G]] =
    successors.map(s => RASIEdge(start, deciding_variables, s))
}
case object RASISuccessor {
  def from[G](successors: Iterable[RASISuccessor[G]]): RASISuccessor[G] =
    RASISuccessor(
      successors.filter(s => s.successors.size > 1)
        .flatMap(s => s.deciding_variables).toSet,
      successors.flatMap(s => s.successors).toSet,
    )
}

case class RASIEdge[G](
    from: AbstractState[G],
    vars: Set[ConcreteVariable[G]],
    to: AbstractState[G],
)
