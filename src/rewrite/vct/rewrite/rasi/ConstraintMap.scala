package vct.rewrite.rasi

case class ConstraintMap[G](constraints: Map[ResolvableVariable[G], Set[UncertainValue]]) {
  /**
   * Concatenates the constraints from two constraint maps into one. This is semantically equivalent to an "and"
   * operation on the two constraint sets.
   *
   * @param other Constraint map to concatenate this one with
   * @return A new constraint map representing the conjunction of the arguments
   */
  def ++(other: ConstraintMap[G]): ConstraintMap[G] = {
    var map: Map[ResolvableVariable[G], Set[UncertainValue]] = constraints
    for (e <- other.constraints) {
      map = map + (e._1 -> (map.getOrElse(e._1, Set()) ++ e._2))
    }
    ConstraintMap(map)
  }

  /**
   * Combines the constraints from two constraint maps into one that represents the disjunction of both inputs. The
   * result will contain only constraints on variables that occur in both maps, and these will be mapped to the union of
   * the sets of possible values they were mapped to in either input map.
   *
   * @param other Constraint map to combine this one with
   * @return A new constraint map representing the disjunction of the arguments
   */
  def ||(other: ConstraintMap[G]): ConstraintMap[G] = {
    val this_values: Map[ResolvableVariable[G], UncertainValue] = this.resolve
    val other_values: Map[ResolvableVariable[G], UncertainValue] = other.resolve
    var map: Map[ResolvableVariable[G], Set[UncertainValue]] = Map.empty[ResolvableVariable[G], Set[UncertainValue]]
    for (e <- this_values.keySet) {
      if (other_values.contains(e)) {
        map += (e -> Set(this_values(e).union(other_values(e))))
      }
    }
    ConstraintMap(map)
  }

  /**
   * Resolves the various constraints placed on each variable in the map and returns a simpler map mapping every
   * variable to exactly one uncertain value.
   *
   * @return A representation of this map in which each variable is mapped to one uncertain value
   */
  def resolve: Map[ResolvableVariable[G], UncertainValue] =
    constraints.map(e => e._1 -> e._2.reduce((v1, v2) => v1.intersection(v2)))

  /**
   * Checks whether this map's valuation is impossible. A valuation is impossible if any variable is mapped to an empty
   * set of potential values.
   *
   * @return <code>true</code> if any variable is mapped to an impossible value, <code>false</code> otherwise
   */
  def is_impossible: Boolean = resolve.exists(t => t._2.is_impossible)
}
case object ConstraintMap {
  def from[G](variable: ResolvableVariable[G], value: UncertainValue): ConstraintMap[G] =
    ConstraintMap(Map.from(Seq(variable -> Set(value))))

  def from_cons[G](cons: Set[(ResolvableVariable[G], UncertainValue)]): ConstraintMap[G] =
    ConstraintMap(Map.from(cons.map(t => t._1 -> Set(t._2))))

  def empty[G]: ConstraintMap[G] = ConstraintMap(Map.empty)

  def impossible[G](vars: Set[_ <: ResolvableVariable[G]]): ConstraintMap[G] =
    ConstraintMap(Map.from(vars.map(v => v -> Set(UncertainValue.empty_of(v.t)))))
}
