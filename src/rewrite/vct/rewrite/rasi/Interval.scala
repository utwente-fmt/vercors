package vct.rewrite.rasi

trait Interval {
  def empty(): Boolean
  def non_empty(): Boolean = !empty()
  def intersection(other: Interval): Interval
  def union(other: Interval): Interval
  def complement(): Interval
  def +(other: Interval): Interval
  def -(other: Interval): Interval
  def *(other: Interval): Interval
  def /(other: Interval): Interval
  def %(other: Interval): Interval
  def unary_- : Interval
  def pow(other:Interval): Interval
  def sub_intervals(): Seq[Interval] = Seq(this)
}

case object EmptyInterval extends Interval {
  override def empty(): Boolean = true
  override def intersection(other: Interval): Interval = this
  override def union(other: Interval): Interval = other
  override def complement(): Interval = UnboundedInterval
  override def +(other: Interval): Interval = this
  override def -(other: Interval): Interval = this
  override def *(other: Interval): Interval = this
  override def /(other: Interval): Interval = this
  override def %(other: Interval): Interval = this
  override def unary_- : Interval = this
  override def pow(other: Interval): Interval = this
}

case class MultiInterval(intervals: Seq[Interval]) extends Interval {
  override def empty(): Boolean = intervals.isEmpty || intervals.forall(i => i.empty())
  override def intersection(other: Interval): Interval = MultiInterval(intervals.map(i => i.intersection(other)))
  override def union(other: Interval): Interval = ???
  override def complement(): Interval = ???
  override def +(other: Interval): Interval = ???
  override def -(other: Interval): Interval = ???
  override def *(other: Interval): Interval = ???
  override def /(other: Interval): Interval = ???
  override def %(other: Interval): Interval = ???
  override def unary_- : Interval = MultiInterval(intervals.map(i => -i))
  override def pow(other: Interval): Interval = ???
  override def sub_intervals(): Seq[Interval] = intervals.flatMap(i => i.sub_intervals())
}

case class BoundedInterval(lower: Int, upper: Int) extends Interval {
  override def empty(): Boolean = lower > upper
  override def intersection(other: Interval): Interval = ???
  override def union(other: Interval): Interval = ???
  override def complement(): Interval =
    MultiInterval(Seq(UpperBoundedInterval(lower - 1), LowerBoundedInterval(upper + 1)))
  override def +(other: Interval): Interval = ???
  override def -(other: Interval): Interval = ???
  override def *(other: Interval): Interval = ???
  override def /(other: Interval): Interval = ???
  override def %(other: Interval): Interval = ???
  override def unary_- : Interval = BoundedInterval(-upper, -lower)
  override def pow(other: Interval): Interval = ???
}

case class LowerBoundedInterval(lower: Int) extends Interval {
  override def empty(): Boolean = false
  override def intersection(other: Interval): Interval = ???
  override def union(other: Interval): Interval = ???
  override def complement(): Interval = UpperBoundedInterval(lower - 1)
  override def +(other: Interval): Interval = ???
  override def -(other: Interval): Interval = ???
  override def *(other: Interval): Interval = ???
  override def /(other: Interval): Interval = ???
  override def %(other: Interval): Interval = ???
  override def unary_- : Interval = UpperBoundedInterval(-lower)
  override def pow(other: Interval): Interval = ???
}

case class UpperBoundedInterval(upper: Int) extends Interval {
  override def empty(): Boolean = false
  override def intersection(other: Interval): Interval = ???
  override def union(other: Interval): Interval = ???
  override def complement(): Interval = LowerBoundedInterval(upper + 1)
  override def +(other: Interval): Interval = ???
  override def -(other: Interval): Interval = ???
  override def *(other: Interval): Interval = ???
  override def /(other: Interval): Interval = ???
  override def %(other: Interval): Interval = ???
  override def unary_- : Interval = LowerBoundedInterval(-upper)
  override def pow(other: Interval): Interval = ???
}

case object UnboundedInterval extends Interval {
  override def empty(): Boolean = false
  override def intersection(other: Interval): Interval = other
  override def union(other: Interval): Interval = this
  override def complement(): Interval = EmptyInterval
  override def +(other: Interval): Interval = this
  override def -(other: Interval): Interval = this
  override def *(other: Interval): Interval = this
  override def /(other: Interval): Interval = this
  override def %(other: Interval): Interval = other match {
    case UnboundedInterval | EmptyInterval => other
    case mi: MultiInterval => {
      val intvs = mi.sub_intervals()
      if (intvs.collect{case LowerBoundedInterval(_) | UpperBoundedInterval(_) | UnboundedInterval => 0}.nonEmpty)
        return this
      val max = intvs.map{case EmptyInterval => 0; case BoundedInterval(lower, upper) => Utils.absmax(lower, upper)}.max - 1
      if (max <= 0) EmptyInterval
      else BoundedInterval(-max, max)
    }
    case BoundedInterval(lower, upper) => {
      val max = Utils.absmax(lower, upper) - 1
      BoundedInterval(-max, max)
    }
    case LowerBoundedInterval(_) => this
    case UpperBoundedInterval(_) => this
  }
  override def unary_- : Interval = this
  override def pow(other: Interval): Interval = this
}