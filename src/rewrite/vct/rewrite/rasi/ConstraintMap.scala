package vct.rewrite.rasi

case class ConstraintMap[G](constraints: Map[ConcreteVariable[G], Set[UncertainValue]]) {
  def ++(other: ConstraintMap[G]): ConstraintMap[G] = {
    var map: Map[ConcreteVariable[G], Set[UncertainValue]] = constraints
    for (e <- other.constraints) {
      map = map + (e._1 -> (map.getOrElse(e._1, Set()) ++ e._2))
    }
    ConstraintMap(map)
  }
  def resolve: Map[ConcreteVariable[G], UncertainValue] = constraints.map(e => e._1 -> e._2.reduce((v1, v2) => v1.intersection(v2)))
}
case object ConstraintMap {
  def from[G](variable: ConcreteVariable[G], value: UncertainValue): ConstraintMap[G] = ConstraintMap(Map.from(Seq(variable -> Set(value))))
  def from_cons[G](cons: Set[(ConcreteVariable[G], UncertainValue)]): ConstraintMap[G] = ConstraintMap(Map.from(cons.map(t => t._1 -> Set(t._2))))
  def empty[G]: ConstraintMap[G] = ConstraintMap(Map.empty)
}
