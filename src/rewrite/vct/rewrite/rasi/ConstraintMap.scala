package vct.rewrite.rasi

case class ConstraintMap[G](constraints: Map[ResolvableVariable[G], Set[UncertainValue]]) {
  def ++(other: ConstraintMap[G]): ConstraintMap[G] = {
    var map: Map[ResolvableVariable[G], Set[UncertainValue]] = constraints
    for (e <- other.constraints) {
      map = map + (e._1 -> (map.getOrElse(e._1, Set()) ++ e._2))
    }
    ConstraintMap(map)
  }
  def resolve: Map[ResolvableVariable[G], UncertainValue] = constraints.map(e => e._1 -> e._2.reduce((v1, v2) => v1.intersection(v2)))
  def is_impossible: Boolean = resolve.exists(t => t._2.is_impossible)
}
case object ConstraintMap {
  def from[G](variable: ResolvableVariable[G], value: UncertainValue): ConstraintMap[G] = ConstraintMap(Map.from(Seq(variable -> Set(value))))
  def from_cons[G](cons: Set[(ResolvableVariable[G], UncertainValue)]): ConstraintMap[G] = ConstraintMap(Map.from(cons.map(t => t._1 -> Set(t._2))))
  def empty[G]: ConstraintMap[G] = ConstraintMap(Map.empty)
}
