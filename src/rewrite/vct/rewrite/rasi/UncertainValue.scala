package vct.rewrite.rasi

import vct.col.ast.{BooleanValue, Expr, IntType, Not, TBool, Type}

trait UncertainValue {
  def can_be_equal(other: UncertainValue): Boolean
  def can_be_unequal(other: UncertainValue): Boolean
  def is_impossible: Boolean
  def is_subset_of(other: UncertainValue): Boolean
  def complement(): UncertainValue
  def intersection(other: UncertainValue): UncertainValue
  def union(other: UncertainValue): UncertainValue
  def to_expression[G](variable: Expr[G]): Expr[G]
  def ==(other: UncertainValue): UncertainBooleanValue
  def !=(other: UncertainValue): UncertainBooleanValue
}
case object UncertainValue {
  def uncertain_of(t: Type[_]): UncertainValue = t match {
    case _: IntType[_] => UncertainIntegerValue.uncertain()
    case _: TBool[_] => UncertainBooleanValue.uncertain()
  }
  def empty_of(t: Type[_]): UncertainValue = t match {
    case _: IntType[_] => UncertainIntegerValue.empty()
    case _: TBool[_] => UncertainBooleanValue.empty()
  }
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

  override def is_impossible: Boolean = !can_be_true && !can_be_false

  override def is_subset_of(other: UncertainValue): Boolean = other match {
    case UncertainBooleanValue(t, f) => (t || !can_be_true) && (f || !can_be_false)
    case _ => false
  }

  override def complement(): UncertainValue = !this

  override def intersection(other: UncertainValue): UncertainValue = other match {
    case UncertainBooleanValue(t, f) => UncertainBooleanValue(can_be_true && t, can_be_false && f)
    case _ => throw new IllegalArgumentException("Trying to intersect boolean with a different type")
  }

  override def union(other: UncertainValue): UncertainValue = other match {
    case UncertainBooleanValue(t, f) => UncertainBooleanValue(can_be_true || t, can_be_false || f)
    case _ => throw new IllegalArgumentException("Trying to union boolean with a different type")
  }

  override def to_expression[G](variable: Expr[G]): Expr[G] = {
    if (can_be_true && can_be_false) BooleanValue(value = true)(variable.o)
    else if (can_be_true) variable
    else if (can_be_false) Not(variable)(variable.o)
    else BooleanValue(value = false)(variable.o)
  }

  override def ==(other: UncertainValue): UncertainBooleanValue = other match {
    case b: UncertainBooleanValue => this == b
    case _ => UncertainBooleanValue.from(false)
  }

  override def !=(other: UncertainValue): UncertainBooleanValue = other match {
    case b: UncertainBooleanValue => this != b
    case _ => UncertainBooleanValue.from(true)
  }

  def try_to_resolve(): Option[Boolean] = {
    if (can_be_true && !can_be_false) Some(true)
    else if (can_be_false && !can_be_true) Some(false)
    else None
  }

  def &&(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true && other.can_be_true, can_be_false || other.can_be_false)
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
case object UncertainBooleanValue {
  def empty(): UncertainBooleanValue = UncertainBooleanValue(can_be_true = false, can_be_false = false)
  def from(bool: Boolean): UncertainBooleanValue = UncertainBooleanValue(bool, !bool)
  def uncertain(): UncertainBooleanValue = UncertainBooleanValue(can_be_true = true, can_be_false = true)
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

  override def is_impossible: Boolean = value.empty()

  override def is_subset_of(other: UncertainValue): Boolean = other match {
    case UncertainIntegerValue(v) => value.is_subset_of(v)
    case _ => false
  }

  override def complement(): UncertainValue = value match {
    case BoundedInterval(lower, upper) if lower == upper => UncertainIntegerValue(value.complement())
    case _ => UncertainIntegerValue.uncertain()
  }

  override def intersection(other: UncertainValue): UncertainValue = other match {
    case UncertainIntegerValue(v) => UncertainIntegerValue(value.intersection(v))
    case _ => throw new IllegalArgumentException("Trying to intersect integer with different type")
  }

  override def union(other: UncertainValue): UncertainValue = other match {
    case UncertainIntegerValue(v) => UncertainIntegerValue(value.union(v))
    case _ => throw new IllegalArgumentException("Trying to union integer with different type")
  }

  override def to_expression[G](variable: Expr[G]): Expr[G] = value.to_expression(variable)

  override def ==(other: UncertainValue): UncertainBooleanValue = other match {
    case i: UncertainIntegerValue => this == i
    case _ => UncertainBooleanValue.from(false)
  }

  override def !=(other: UncertainValue): UncertainBooleanValue = other match {
    case i: UncertainIntegerValue => this != i
    case _ => UncertainBooleanValue.from(true)
  }

  def try_to_resolve(): Option[Int] = value.try_to_resolve()

  def below_eq(): UncertainIntegerValue = UncertainIntegerValue(value.below_max())
  def below(): UncertainIntegerValue = below_eq() + UncertainIntegerValue.single(-1)
  def above_eq(): UncertainIntegerValue = UncertainIntegerValue(value.above_min())
  def above(): UncertainIntegerValue = above_eq() + UncertainIntegerValue.single(1)

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

  def unary_- : UncertainIntegerValue = UncertainIntegerValue(-value)
  def +(other: UncertainIntegerValue): UncertainIntegerValue = UncertainIntegerValue(value + other.value)
  def -(other: UncertainIntegerValue): UncertainIntegerValue = UncertainIntegerValue(value - other.value)
  def *(other: UncertainIntegerValue): UncertainIntegerValue = UncertainIntegerValue(value * other.value)
  def /(other: UncertainIntegerValue): UncertainIntegerValue = UncertainIntegerValue.uncertain() // TODO: UncertainIntegerValue(value / other.value)
  def %(other: UncertainIntegerValue): UncertainIntegerValue = UncertainIntegerValue.uncertain() // TODO: UncertainIntegerValue(value % other.value)
  def pow(other: UncertainIntegerValue): UncertainIntegerValue = UncertainIntegerValue.uncertain() // TODO: UncertainIntegerValue(value.pow(other.value))
}
case object UncertainIntegerValue {
  def empty(): UncertainIntegerValue = UncertainIntegerValue(EmptyInterval)
  def uncertain(): UncertainIntegerValue = UncertainIntegerValue(UnboundedInterval)
  def above(int: Int): UncertainIntegerValue = UncertainIntegerValue(LowerBoundedInterval(int))
  def single(int: Int): UncertainIntegerValue = UncertainIntegerValue(BoundedInterval(int, int))
}

case class UncertainSequence(len: UncertainIntegerValue, values: Seq[(UncertainIntegerValue, UncertainValue)], t: Type[_]) {
  def union(other: UncertainSequence): UncertainSequence = {
    if (t != other.t) throw new IllegalArgumentException("Unioning sequences of different types")
    UncertainSequence(len.union(other.len).asInstanceOf[UncertainIntegerValue], Utils.combine_values(values, other.values), t)
  }

  def concat(other: UncertainSequence): UncertainSequence =
    UncertainSequence(len + other.len, values ++ other.values.map(t => (t._1 + len) -> t._2), t)

  def prepend(value: UncertainValue): UncertainSequence =
    UncertainSequence(len + UncertainIntegerValue.single(1), (UncertainIntegerValue.single(0) -> value) +: values, t)

  def updated(index: UncertainIntegerValue, value: UncertainValue): UncertainSequence =
    UncertainSequence(len, values.filter(t => !t._1.can_be_equal(index)) :+ index -> value, t)

  def remove(index: UncertainIntegerValue): UncertainSequence =
    UncertainSequence(len - UncertainIntegerValue.single(1), values.filter(t => !t._1.can_be_equal(index)), t)

  def take(num: UncertainIntegerValue): UncertainSequence =
    UncertainSequence(num, values.filter(t => t._1.<(num).can_be_true).map(t => t._1.intersection(num.below()).asInstanceOf[UncertainIntegerValue] -> t._2), t)

  def drop(num: UncertainIntegerValue): UncertainSequence = {
    val red: UncertainIntegerValue = num.intersection(len.below()).asInstanceOf[UncertainIntegerValue]
    UncertainSequence(len - red, values.filter(t => t._1.>=(red).can_be_true).map(t => t._1.intersection(red.above_eq()).asInstanceOf[UncertainIntegerValue] -> t._2), t)
  }

  def slice(lower: UncertainIntegerValue, upper: UncertainIntegerValue): UncertainSequence =
    take(upper).drop(lower)

  def get(index: Int): UncertainValue = {
    if (index < 0) throw new IllegalArgumentException(s"Trying to access negative index $index")
    val i = values.indexWhere(t => t._1.try_to_resolve().getOrElse(-1) == index)
    if (i >= 0) values(i)._2
    else UncertainValue.uncertain_of(t)
  }
}
case object UncertainSequence {
  def empty(t: Type[_]): UncertainSequence = UncertainSequence(UncertainIntegerValue.empty(), Seq(), t)
}