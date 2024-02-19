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

  def &&(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true && other.can_be_true, can_be_false || other.can_be_false)

  def ||(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true || other.can_be_true, can_be_false && other.can_be_false)

  def !(): UncertainBooleanValue =
    UncertainBooleanValue(can_be_false, can_be_true)

  def ^(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true && other.can_be_false || can_be_false && other.can_be_true, can_be_true && other.can_be_true || can_be_false && other.can_be_false)
}

case class UncertainIntegerValue() extends UncertainValue {
  override def can_be_equal(other: UncertainValue): Boolean = ???

  override def can_be_unequal(other: UncertainValue): Boolean = ???
}
