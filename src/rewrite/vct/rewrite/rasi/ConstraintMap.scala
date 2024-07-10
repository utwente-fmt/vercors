package vct.rewrite.rasi

case class ConstraintMap[G](
    constraints: Map[ResolvableVariable[G], UncertainValue]
) {

  /** Concatenates the constraints from two constraint maps into one. This is
    * semantically equivalent to an "and" operation on the two constraint sets.
    *
    * @param other
    *   Constraint map to concatenate this one with
    * @return
    *   A new constraint map representing the conjunction of the arguments
    */
  def &&(other: ConstraintMap[G]): ConstraintMap[G] =
    ConstraintMap(constraints ++ other.constraints.map { case (k, v) =>
      k -> v.intersection(
        constraints.getOrElse(k, UncertainValue.uncertain_of(v.t[G]))
      )
    })

  /** Combines the constraints from two constraint maps into one that represents
    * the disjunction of both inputs. The result will contain only constraints
    * on variables that occur in both maps, and these will be mapped to the
    * union of the sets of possible values they were mapped to in either input
    * map.
    *
    * @param other
    *   Constraint map to combine this one with
    * @return
    *   A new constraint map representing the disjunction of the arguments
    */
  def ||(other: ConstraintMap[G]): ConstraintMap[G] =
    ConstraintMap.from_cons(
      constraints.keySet.intersect(other.constraints.keySet)
        .map(k => k -> constraints(k).union(other.constraints(k)))
    )

  /** Resolves the various constraints placed on each variable in the map and
    * returns a simpler map mapping every variable to exactly one uncertain
    * value.
    *
    * @return
    *   A representation of this map in which each variable is mapped to one
    *   uncertain value
    */
  def resolve: Map[ResolvableVariable[G], UncertainValue] = constraints

  /** Checks whether this map's valuation is impossible. A valuation is
    * impossible if any variable is mapped to an empty set of potential values.
    *
    * @return
    *   <code>true</code> if any variable is mapped to an impossible value,
    *   <code>false</code> otherwise
    */
  def is_impossible: Boolean = constraints.exists(t => t._2.is_impossible)

  /** Checks whether the constraint map is empty. An empty map represents the
    * constraint of all variables to the uncertain value of their respective
    * type. As such, a map containing only perfectly uncertain valuations is
    * also considered empty.
    *
    * @return
    *   <code>true</code> if the map does not constrain any variable,
    *   <code>false</code> otherwise
    */
  def is_empty: Boolean = resolve.forall(v => v._2.is_uncertain)
}
case object ConstraintMap {
  def from[G](
      variable: ResolvableVariable[G],
      value: UncertainValue,
  ): ConstraintMap[G] = ConstraintMap(Map.from(Seq(variable -> value)))

  def from_cons[G](
      cons: Set[(ResolvableVariable[G], UncertainValue)]
  ): ConstraintMap[G] = ConstraintMap(Map.from(cons.map(t => t._1 -> t._2)))

  def empty[G]: ConstraintMap[G] = ConstraintMap(Map.empty)

  def impossible[G](vars: Set[_ <: ResolvableVariable[G]]): ConstraintMap[G] =
    ConstraintMap(Map.from(vars.map(v => v -> UncertainValue.empty_of(v.t))))
}
