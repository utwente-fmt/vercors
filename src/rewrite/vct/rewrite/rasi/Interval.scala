package vct.rewrite.rasi

import vct.col.ast._

sealed abstract class IntervalSize {
  def get: Int
  def +(other: IntervalSize): IntervalSize = this match {
    case Infinite() => Infinite()
    case Finite(v1) => other match {
      case Infinite() => Infinite()
      case Finite(v2) => Finite(v1 + v2)
    }
  }
  def >=(other: IntervalSize): Boolean = this match {
    case Infinite() => true
    case Finite(v1) => other match {
      case Infinite() => false
      case Finite(v2) => v1 >= v2
    }
  }
}
case class Finite(value: Int) extends IntervalSize {
  override def get: Int = value
}
case class Infinite() extends IntervalSize {
  override def get: Int = throw new NoSuchElementException("Accessing infinite interval size element")
}

/**
 * An interval represents a set of integers. It can be empty (type <code>EmptyInterval</code>), unbounded (type
 * <code>UnboundedInterval</code>), bounded by two integers (type <code>BoundedInterval</code>), below an upper bound
 * (type <code>UpperBoundedInterval</code>), above a lower bound (type <code>LowerBoundedInterval</code>) or a union of
 * a finite set of other intervals (type <code>MultiInterval</code>). If it is bounded, then the bounds are contained in
 * the interval.
 *
 * An interval supports the following operations:
 *
 * SIZE OPERATIONS
 *   - <code>empty()</code> returns <code>true</code> if the interval is empty and <code>false</code> otherwise
 *   - <code>non_empty()</code> returns <code>false</code> if the interval is empty and <code>true</code> otherwise
 *   - <code>size()</code> returns the size of the interval in the form of <code>Finite(n)</code> if the interval is
 *   finite and of size <code>n</code>, or <code>Infinite()</code> otherwise
 *
 * SET OPERATIONS
 *   - <code>intersection(Interval)</code> returns the intersection between this interval and the argument
 *   - <code>union(Interval)</code> returns the union of this interval and the argument
 *   - <code>complement()</code> returns the interval representing all integers that are not contained in this interval
 *
 * ARITHMETIC OPERATIONS
 *   - <code>below_max()</code> returns an interval whose upper bound is the maximum entry in this interval, or an
 *   unbounded interval if there is no maximum
 *   - <code>above_min()</code> returns an interval whose lower bound is the minimum entry in this interval, or an
 *   unbounded interval if there is no minimum
 *   - <code>+</code> returns the interval representing the set of sums of entries in both intervals
 *   - <code>-</code> returns the interval representing the set of subtractions of entries of the second interval from
 *   entries in the first
 *   - <code>*</code> returns the interval overapproximating the set of products from both intervals. Since this is not
 *   generally a contiguous interval, this overapproximates to the interval between the minimum and maximum product or,
 *   if multiple such intervals can be defined, to the union of these intervals
 *   - <code>unary_-</code> returns the element-wise negation of this interval
 *   - <code>/</code> NOT IMPLEMENTED
 *   - <code>%</code> NOT IMPLEMENTED
 *   - <code>pow</code> NOT IMPLEMENTED
 *
 * ADMINISTRATIVE OPERATIONS
 *   - <code>sub_intervals()</code> returns the minimum set of contiguous intervals this interval is a union of
 *   - <code>try_to_resolve()</code> if this interval contains exactly one integer, returns this integer, otherwise
 *   returns <code>None</code>
 *   - <code>to_expression(Variable)</code> returns an expression indicating that the given variable is within this
 *   interval
 */
sealed abstract class Interval {
  def empty(): Boolean
  def non_empty(): Boolean = !empty()
  def size(): IntervalSize
  def intersection(other: Interval): Interval
  def union(other: Interval): Interval
  def complement(): Interval
  def is_subset_of(other: Interval): Boolean
  def below_max(): Interval
  def above_min(): Interval
  def +(other: Interval): Interval
  def -(other: Interval): Interval = this.+(-other)
  def *(other: Interval): Interval    // TODO: Modeling the multiple of an interval as an interval is imprecise
  def /(other: Interval): Interval
  def %(other: Interval): Interval
  def unary_- : Interval
  def pow(other:Interval): Interval
  def sub_intervals(): Set[Interval] = Set(this)
  def try_to_resolve(): Option[Int]
  def to_expression[G](variable: Expr[G]): Expr[G]    // TODO: Use proper origin
}

case object EmptyInterval extends Interval {
  override def empty(): Boolean = true
  override def size(): IntervalSize = Finite(0)
  override def intersection(other: Interval): Interval = this
  override def union(other: Interval): Interval = other
  override def complement(): Interval = UnboundedInterval
  override def is_subset_of(other: Interval): Boolean = true
  override def below_max(): Interval = this
  override def above_min(): Interval = this
  override def +(other: Interval): Interval = this
  override def *(other: Interval): Interval = this
  override def /(other: Interval): Interval = this
  override def %(other: Interval): Interval = this
  override def unary_- : Interval = this
  override def pow(other: Interval): Interval = this
  override def try_to_resolve(): Option[Int] = None
  override def to_expression[G](variable: Expr[G]): Expr[G] = BooleanValue(value = false)(variable.o)
}

case class MultiInterval(intervals: Set[Interval]) extends Interval {
  override def empty(): Boolean = intervals.isEmpty || intervals.forall(i => i.empty())

  override def size(): IntervalSize = intervals.toSeq.map(i => i.size()).reduce((s1, s2) => s1 + s2)

  override def intersection(other: Interval): Interval = MultiInterval(merge_intersecting(intervals.map(i => i.intersection(other))))

  override def union(other: Interval): Interval = {
    val (intersecting, non_intersecting) = intervals.partition(i => i.intersection(other).non_empty())
    // Merge together intervals that are connected by the new interval
    val new_intervals = non_intersecting + intersecting.fold(other)((i1, i2) => i1.union(i2))
    // It could be that all intervals are now connected into one
    if (new_intervals.size > 1) MultiInterval(new_intervals)
    else new_intervals.head
  }

  override def complement(): Interval = intervals.foldLeft[Interval](UnboundedInterval)((i1, i2) => i1.intersection(i2.complement()))

  override def is_subset_of(other: Interval): Boolean = intervals.forall(p => p.is_subset_of(other))

  override def below_max(): Interval = intervals.foldLeft[Interval](EmptyInterval)((i1, i2) => i1.union(i2.below_max()))

  override def above_min(): Interval = intervals.foldLeft[Interval](EmptyInterval)((i1, i2) => i1.union(i2.above_min()))

  override def +(other: Interval): Interval = {
    val new_intervals = intervals.map(i => i + other)
    // It could be that all intervals are now connected into one
    if (new_intervals.size > 1) MultiInterval(new_intervals)
    else new_intervals.head
  }

  override def *(other: Interval): Interval = {
    val new_intervals = intervals.map(i => i * other)
    // It could be that all intervals are now connected into one
    if (new_intervals.size > 1) MultiInterval(new_intervals)
    else new_intervals.head
  }

  override def /(other: Interval): Interval = {
    var new_intervals = intervals.map(i => i / other)
    new_intervals = merge_intersecting(new_intervals)
    // It could be that all intervals are now connected into one
    if (new_intervals.size > 1) MultiInterval(new_intervals)
    else new_intervals.head
  }

  override def %(other: Interval): Interval = {
    var new_intervals = intervals.map(i => i % other)
    new_intervals = merge_intersecting(new_intervals)
    // It could be that all intervals are now connected into one
    if (new_intervals.size > 1) MultiInterval(new_intervals)
    else new_intervals.head
  }

  override def unary_- : Interval = MultiInterval(intervals.map(i => -i))

  override def pow(other: Interval): Interval = {
    val new_intervals = intervals.map(i => i.pow(other))
    // It could be that all intervals are now connected into one
    if (new_intervals.size > 1) MultiInterval(new_intervals)
    else new_intervals.head
  }

  private def merge_intersecting(is: Set[Interval]): Set[Interval] = MultiInterval(is).sub_intervals().reduce((i1, i2) => i1.union(i2)).sub_intervals()

  override def sub_intervals(): Set[Interval] = intervals.flatMap(i => i.sub_intervals())

  override def try_to_resolve(): Option[Int] = {
    if (intervals.count(i => i != EmptyInterval) == 1) intervals.filter(i => i != EmptyInterval).head.try_to_resolve()
    else None
  }

  override def to_expression[G](variable: Expr[G]): Expr[G] = {
    intervals.map(i => i.to_expression(variable)).reduce((e1, e2) => Or(e1, e2)(variable.o))
  }
}

case class BoundedInterval(lower: Int, upper: Int) extends Interval {
  override def empty(): Boolean = lower > upper

  override def size(): IntervalSize = Finite(scala.math.max(upper - lower + 1, 0))

  override def intersection(other: Interval): Interval = other match {
    case EmptyInterval => other
    case mi: MultiInterval => mi.intersection(this)
    case BoundedInterval(low, up) =>
      if (up <= upper && up >= lower || low <= upper && low >= lower) BoundedInterval(scala.math.max(low, lower), scala.math.min(up, upper))
      else EmptyInterval
    case LowerBoundedInterval(low) =>
      if (upper >= low) BoundedInterval(scala.math.max(low, lower), upper)
      else EmptyInterval
    case UpperBoundedInterval(up) =>
      if (lower <= up) BoundedInterval(lower, scala.math.min(up, upper))
      else EmptyInterval
    case UnboundedInterval => this
  }

  override def union(other: Interval): Interval = other match {
    case EmptyInterval => this
    case mi: MultiInterval => mi.union(this)
    case BoundedInterval(low, up) =>
      if (up <= upper + 1 && up >= lower - 1 || low <= upper + 1 && low >= lower - 1) BoundedInterval(scala.math.min(low, lower), scala.math.max(up, upper))
      else MultiInterval(Set(this, other))
    case LowerBoundedInterval(low) =>
      if (upper + 1 >= low) LowerBoundedInterval(scala.math.min(low, lower))
      else MultiInterval(Set(this, other))
    case UpperBoundedInterval(up) =>
      if (lower - 1 <= up) UpperBoundedInterval(scala.math.max(up, upper))
      else MultiInterval(Set(this, other))
    case UnboundedInterval => other
  }

  override def complement(): Interval =
    MultiInterval(Set(UpperBoundedInterval(lower - 1), LowerBoundedInterval(upper + 1)))

  override def is_subset_of(other: Interval): Boolean = empty() || (other match {
    case EmptyInterval => false
    case MultiInterval(intervals) => intervals.exists(p => is_subset_of(p))
    case BoundedInterval(low, up) => low <= lower && up >= upper
    case LowerBoundedInterval(low) => low <= lower
    case UpperBoundedInterval(up) => up >= upper
    case UnboundedInterval => true
  })

  override def below_max(): Interval = UpperBoundedInterval(upper)

  override def above_min(): Interval = LowerBoundedInterval(lower)

  override def +(other: Interval): Interval = other match {
    case EmptyInterval | UnboundedInterval => other
    case mi: MultiInterval => mi.+(this)
    case BoundedInterval(low, up) => BoundedInterval(lower + low, upper + up)
    case LowerBoundedInterval(low) => LowerBoundedInterval(lower + low)
    case UpperBoundedInterval(up) => UpperBoundedInterval(upper + up)
  }

  override def *(other: Interval): Interval = other match {
    case EmptyInterval | UnboundedInterval => other
    case mi: MultiInterval => mi.*(this)
    case BoundedInterval(low, up) => BoundedInterval(Utils.prod_min(low, up, lower, upper), Utils.prod_max(low, up, lower, upper))
    case LowerBoundedInterval(low) =>
      if (lower < 0 && upper > 0) UnboundedInterval
      else if (lower >= 0) LowerBoundedInterval(scala.math.min(low * upper, low * lower))
      else UpperBoundedInterval(scala.math.max(low * upper, low * lower))
    case UpperBoundedInterval(up) =>
      if (lower < 0 && upper > 0) UnboundedInterval
      else if (lower >= 0) UpperBoundedInterval(scala.math.max(up * lower, up * upper))
      else LowerBoundedInterval(scala.math.min(up * lower, up * upper))
  }

  override def /(other: Interval): Interval = other match {
    case EmptyInterval => other
    case MultiInterval(intervals) => ???
    case BoundedInterval(low, up) => ???
    case LowerBoundedInterval(low) =>
      if (low < 0) BoundedInterval(-Utils.abs_max(lower, upper), Utils.abs_max(lower, upper))
      else BoundedInterval(scala.math.min(lower / low, 0), scala.math.max(upper / low, 0))
    case UpperBoundedInterval(up) => ???
    case UnboundedInterval => BoundedInterval(-Utils.abs_max(lower, upper), Utils.abs_max(lower, upper))
  }

  override def %(other: Interval): Interval = other match {
    case EmptyInterval => other
    case MultiInterval(intervals) => ???
    case BoundedInterval(low, up) => ???
    case LowerBoundedInterval(low) => ???
    case UpperBoundedInterval(up) => ???
    case UnboundedInterval =>
      if (lower < 0) BoundedInterval(lower, scala.math.max(0, upper))
      else BoundedInterval(0, upper)
  }

  override def unary_- : Interval = BoundedInterval(-upper, -lower)

  override def pow(other: Interval): Interval = other match {
    case EmptyInterval => other
    case MultiInterval(intervals) => ???
    case BoundedInterval(low, up) => ???
    case LowerBoundedInterval(low) => ???
    case UpperBoundedInterval(up) => ???
    case UnboundedInterval =>
      if (lower < -1) other
      else if (lower == -1) LowerBoundedInterval(-1)
      else LowerBoundedInterval(0)
  }

  override def try_to_resolve(): Option[Int] = {
    if (lower == upper) Some(upper)
    else None
  }

  override def to_expression[G](variable: Expr[G]): Expr[G] = {
    if (lower == upper) Eq(variable, IntegerValue(upper)(variable.o))(variable.o)
    else And(LessEq(variable, IntegerValue(upper)(variable.o))(variable.o), GreaterEq(variable, IntegerValue(lower)(variable.o))(variable.o))(variable.o)
  }
}

case class LowerBoundedInterval(lower: Int) extends Interval {
  override def empty(): Boolean = false

  override def size(): IntervalSize = Infinite()

  override def intersection(other: Interval): Interval = other match {
    case EmptyInterval => other
    case mi: MultiInterval => mi.intersection(this)
    case BoundedInterval(low, up) =>
      if (up >= lower) BoundedInterval(scala.math.max(lower, low), up)
      else EmptyInterval
    case LowerBoundedInterval(low) => LowerBoundedInterval(scala.math.max(low, lower))
    case UpperBoundedInterval(up) =>
      if (up >= lower) BoundedInterval(lower, up)
      else EmptyInterval
    case UnboundedInterval => this
  }

  override def union(other: Interval): Interval = other match {
    case EmptyInterval => this
    case mi: MultiInterval => mi.union(this)
    case BoundedInterval(low, up) =>
      if (up + 1 >= lower) LowerBoundedInterval(scala.math.min(low, lower))
      else MultiInterval(Set(other, this))
    case LowerBoundedInterval(low) => LowerBoundedInterval(scala.math.min(low, lower))
    case UpperBoundedInterval(up) =>
      if (up + 1 >= lower) UnboundedInterval
      else MultiInterval(Set(other, this))
    case UnboundedInterval => other
  }

  override def complement(): Interval = UpperBoundedInterval(lower - 1)

  override def is_subset_of(other: Interval): Boolean = other match {
    case EmptyInterval | BoundedInterval(_, _) | UpperBoundedInterval(_) => false
    case MultiInterval(intervals) => intervals.exists(p => is_subset_of(p))
    case LowerBoundedInterval(low) => low <= lower
    case UnboundedInterval => true
  }

  override def below_max(): Interval = UnboundedInterval

  override def above_min(): Interval = this

  override def +(other: Interval): Interval = other match {
    case EmptyInterval | UnboundedInterval => other
    case mi: MultiInterval => mi.+(this)
    case BoundedInterval(low, _) => LowerBoundedInterval(lower + low)
    case LowerBoundedInterval(low) => LowerBoundedInterval(lower + low)
    case UpperBoundedInterval(_) => UnboundedInterval
  }

  override def *(other: Interval): Interval = other match {
    case EmptyInterval | UnboundedInterval => other
    case mi: MultiInterval => mi.*(this)
    case bi: BoundedInterval => bi.*(this)
    case LowerBoundedInterval(low) =>
      if (low < 0 || lower < 0) UnboundedInterval
      else LowerBoundedInterval(low * lower)
    case UpperBoundedInterval(up) =>
      if (lower < 0 || up > 0) UnboundedInterval
      else UpperBoundedInterval(up * lower)
  }

  override def /(other: Interval): Interval = ???

  override def %(other: Interval): Interval = ???

  override def unary_- : Interval = UpperBoundedInterval(-lower)

  override def pow(other: Interval): Interval = ???

  override def try_to_resolve(): Option[Int] = None

  override def to_expression[G](variable: Expr[G]): Expr[G] = GreaterEq(variable, IntegerValue(lower)(variable.o))(variable.o)
}

case class UpperBoundedInterval(upper: Int) extends Interval {
  override def empty(): Boolean = false

  override def size(): IntervalSize = Infinite()

  override def intersection(other: Interval): Interval = other match {
    case EmptyInterval => other
    case mi: MultiInterval => mi.intersection(this)
    case BoundedInterval(low, up) =>
      if (low <= upper) BoundedInterval(low, scala.math.min(up, upper))
      else EmptyInterval
    case LowerBoundedInterval(low) =>
      if (low <= upper) BoundedInterval(low, upper)
      else EmptyInterval
    case UpperBoundedInterval(up) => UpperBoundedInterval(scala.math.min(up, upper))
    case UnboundedInterval => this
  }

  override def union(other: Interval): Interval = other match {
    case EmptyInterval => this
    case mi: MultiInterval => mi.union(this)
    case BoundedInterval(low, up) =>
      if (low - 1 <= upper) UpperBoundedInterval(scala.math.max(upper, up))
      else MultiInterval(Set(this, other))
    case LowerBoundedInterval(low) =>
      if (low - 1 <= upper) UnboundedInterval
      else MultiInterval(Set(this, other))
    case UpperBoundedInterval(up) => UpperBoundedInterval(scala.math.max(upper, up))
    case UnboundedInterval => other
  }

  override def complement(): Interval = LowerBoundedInterval(upper + 1)

  override def is_subset_of(other: Interval): Boolean = other match {
    case EmptyInterval | BoundedInterval(_, _) | LowerBoundedInterval(_) => false
    case MultiInterval(intervals) => intervals.exists(p => is_subset_of(p))
    case UpperBoundedInterval(up) => up >= upper
    case UnboundedInterval => true
  }

  override def below_max(): Interval = this

  override def above_min(): Interval = UnboundedInterval

  override def +(other: Interval): Interval = other match {
    case EmptyInterval | UnboundedInterval => other
    case mi: MultiInterval => mi.+(this)
    case BoundedInterval(_, up) => UpperBoundedInterval(upper + up)
    case LowerBoundedInterval(_) => UnboundedInterval
    case UpperBoundedInterval(up) => UpperBoundedInterval(upper + up)
  }

  override def *(other: Interval): Interval = other match {
    case EmptyInterval | UnboundedInterval => other
    case mi: MultiInterval => mi.*(this)
    case bi: BoundedInterval => bi.*(this)
    case LowerBoundedInterval(low) =>
      if (low < 0 || upper > 0) UnboundedInterval
      else UpperBoundedInterval(low * upper)
    case UpperBoundedInterval(up) =>
      if (up > 0 || upper > 0) UnboundedInterval
      else LowerBoundedInterval(up * upper)
  }

  override def /(other: Interval): Interval = ???

  override def %(other: Interval): Interval = ???

  override def unary_- : Interval = LowerBoundedInterval(-upper)

  override def pow(other: Interval): Interval = ???

  override def try_to_resolve(): Option[Int] = None

  override def to_expression[G](variable: Expr[G]): Expr[G] = LessEq(variable, IntegerValue(upper)(variable.o))(variable.o)
}

case object UnboundedInterval extends Interval {
  override def empty(): Boolean = false
  override def size(): IntervalSize = Infinite()
  override def intersection(other: Interval): Interval = other
  override def union(other: Interval): Interval = this
  override def complement(): Interval = EmptyInterval
  override def is_subset_of(other: Interval): Boolean = other == this
  override def below_max(): Interval = this
  override def above_min(): Interval = this
  override def +(other: Interval): Interval = this
  override def *(other: Interval): Interval = this
  override def /(other: Interval): Interval = this
  override def %(other: Interval): Interval = other match {
    case UnboundedInterval | EmptyInterval => other
    case mi: MultiInterval =>
      val intvs = mi.sub_intervals()
      if (intvs.collect{case LowerBoundedInterval(_) | UpperBoundedInterval(_) | UnboundedInterval => 0}.nonEmpty)
        return this
      val max = intvs.map{case EmptyInterval => 0; case BoundedInterval(lower, upper) => Utils.abs_max(lower, upper)}.max - 1
      if (max <= 0) EmptyInterval
      else BoundedInterval(-max, max)
    case BoundedInterval(lower, upper) =>
      val max = Utils.abs_max(lower, upper) - 1
      BoundedInterval(-max, max)
    case LowerBoundedInterval(_) => this
    case UpperBoundedInterval(_) => this
  }
  override def unary_- : Interval = this
  override def pow(other: Interval): Interval = this
  override def try_to_resolve(): Option[Int] = None
  override def to_expression[G](variable: Expr[G]): Expr[G] = BooleanValue(value = true)(variable.o)
}