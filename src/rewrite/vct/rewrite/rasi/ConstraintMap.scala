package vct.rewrite.rasi

case class ConstraintMap[G](constraints: Map[ResolvableVariable[G], Constraint]) {
  def ||(other: ConstraintMap[G]): ConstraintMap[G] = {
    var map: Map[ResolvableVariable[G], Constraint] = Map.empty[ResolvableVariable[G], Constraint]
    // If a variable is in both constraint maps, use the disjunction. If it is only in one of them, use an uncertain value instead.
    for (e <- constraints) {
      map = map + (e._1 -> (other.constraints.getOrElse(e._1, Constraint.from(UncertainValue.uncertain_of(e._1.t))) || e._2))
    }
    for (e <- other.constraints) {
      if (!map.contains(e._1)) map = map + (e._1 -> Constraint.from(UncertainValue.uncertain_of(e._1.t)))
    }
    ConstraintMap(map)
  }

  def &&(other: ConstraintMap[G]): ConstraintMap[G] = {
    var map: Map[ResolvableVariable[G], Constraint] = Map.empty[ResolvableVariable[G], Constraint]
    // If a variable is in both constraint maps, use the conjunction. Otherwise, use the constraint of the map it is in.
    for (e <- constraints) {
      if (other.constraints.contains(e._1)) map = map + (e._1 -> (other.constraints(e._1) && e._2))
      else map = map + e
    }
    for (e <- other.constraints) {
      if (!map.contains(e._1)) map = map + e
    }
    ConstraintMap(map)
  }

  def resolve: Map[ResolvableVariable[G], UncertainValue] = constraints.map(e => e._1 -> e._2.resolve)

  def is_impossible: Boolean = resolve.exists(t => t._2.is_impossible)
}
case object ConstraintMap {
  def from[G](variable: ResolvableVariable[G], value: UncertainValue): ConstraintMap[G] =
    ConstraintMap(Map.from(Seq(variable -> Constraint.from(value))))

  def from_cons[G](cons: Set[(ResolvableVariable[G], UncertainValue)]): ConstraintMap[G] =
    ConstraintMap(Map.from(cons.map(t => t._1 -> Constraint.from(t._2))))

  def empty[G]: ConstraintMap[G] =
    ConstraintMap(Map.empty)

  def impossible[G](vars: Set[_ <: ResolvableVariable[G]]): ConstraintMap[G] =
    ConstraintMap(Map.from(vars.map(v => v -> Constraint.from(UncertainValue.empty_of(v.t)))))
}

case class Constraint(constraints: Set[Set[UncertainValue]]) {
  def ||(other: Constraint): Constraint =
    Constraint(constraints ++ other.constraints)

  def &&(other: Constraint): Constraint =
    Constraint(constraints.flatMap(s1 => other.constraints.map(s2 => s1 ++ s2)))

  def resolve: UncertainValue =
    constraints.map(s => s.reduce((v1, v2) => v1.intersection(v2))).reduce((v1, v2) => v1.union(v2))
}
case object Constraint {
  def from(value: UncertainValue): Constraint =
    Constraint(Set(Set(value)))
}