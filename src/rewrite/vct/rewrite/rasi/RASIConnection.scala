package vct.rewrite.rasi

case class RASISuccessors[G](deciding_variables: Set[ConcreteVariable[G]], successors: Set[AbstractState[G]])
case class RASIEdge[G](from: AbstractState[G], to: RASISuccessors[G])
