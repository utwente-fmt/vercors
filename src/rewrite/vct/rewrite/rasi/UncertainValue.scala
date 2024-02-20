package vct.rewrite.rasi

trait UncertainValue {
  def can_be_equal(other: UncertainValue): Boolean
  def can_be_unequal(other: UncertainValue): Boolean
}

case class UncertainBooleanValue(can_be_true: Boolean, can_be_false: Boolean) extends UncertainValue {
  override def can_be_equal(other: UncertainValue): Boolean = other match {
    case UncertainBooleanValue(t, f) => can_be_true && t || can_be_false && f
    case _ => false
  }

  override def can_be_unequal(other: UncertainValue): Boolean = other match {
    case UncertainBooleanValue(t, f) => can_be_true && f || can_be_false && t
    case _ => true
  }

  def try_to_resolve(): Option[Boolean] = {
    if (can_be_true && !can_be_false) Some(true)
    else if (can_be_false && !can_be_true) Some(false)
    else None
  }

  def union(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true || other.can_be_true, can_be_false || other.can_be_false)

  def &(other: UncertainBooleanValue): UncertainBooleanValue = this && other
  def &&(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true && other.can_be_true, can_be_false || other.can_be_false)
  def |(other: UncertainBooleanValue): UncertainBooleanValue = this || other
  def ||(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true || other.can_be_true, can_be_false && other.can_be_false)
  def unary_! : UncertainBooleanValue =
    UncertainBooleanValue(can_be_false, can_be_true)
  def ^(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true && other.can_be_false || can_be_false && other.can_be_true,
                          can_be_true && other.can_be_true || can_be_false && other.can_be_false)
  def ==(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true && other.can_be_true || can_be_false && other.can_be_false,
                          can_be_true && other.can_be_false || can_be_false && other.can_be_true)
  def !=(other: UncertainBooleanValue): UncertainBooleanValue = this ^ other
}

case class UncertainIntegerValue(value: Interval) extends UncertainValue {
  override def can_be_equal(other: UncertainValue): Boolean = other match {
    case UncertainIntegerValue(v) => value.intersection(v).non_empty()
    case _ => false
  }

  override def can_be_unequal(other: UncertainValue): Boolean = other match {
    case UncertainIntegerValue(v) => value.intersection(v).complement().non_empty()
    case _ => true
  }

  def try_to_resolve(): Option[Int] = value.try_to_resolve()

  def union(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value.union(other.value))

  def ==(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_equal(other), can_be_unequal(other))
  def !=(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_unequal(other), can_be_equal(other))
  def >=(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(value.below_max().intersection(other.value).non_empty(),
                          value.above_min().intersection(other.value).size() >= Finite(1))
  def <=(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(value.above_min().intersection(other.value).non_empty(),
                          value.below_max().intersection(other.value).size() >= Finite(1))
  def >(other: UncertainIntegerValue): UncertainBooleanValue = !(this <= other)
  def <(other: UncertainIntegerValue): UncertainBooleanValue = !(this >= other)

  def unary_- : UncertainIntegerValue =
    UncertainIntegerValue(-value)
  def +(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value + other.value)
  def -(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value - other.value)
  def *(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value * other.value)
  def /(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value / other.value)
  def %(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value % other.value)
  def pow(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value.pow(other.value))
}
case object UncertainIntegerValue {
  def empty(): UncertainIntegerValue = UncertainIntegerValue(EmptyInterval)
  def uncertain(): UncertainIntegerValue = UncertainIntegerValue(UnboundedInterval)
  def above(int: Int): UncertainIntegerValue = UncertainIntegerValue(LowerBoundedInterval(int))
  def single(int: Int): UncertainIntegerValue = UncertainIntegerValue(BoundedInterval(int, int))
}