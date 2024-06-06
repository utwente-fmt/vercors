package vct.rewrite.rasi

import vct.col.ast.{BooleanValue, Expr, IntType, Not, TBool, TInt, Type}

// TODO: Factor out into uncertain single value and uncertain collection value
trait UncertainValue {
  def can_be_equal(other: UncertainValue): Boolean
  def can_be_unequal(other: UncertainValue): Boolean
  def is_uncertain: Boolean
  def is_impossible: Boolean
  def is_subset_of(other: UncertainValue): Boolean
  def complement(): UncertainValue
  def intersection(other: UncertainValue): UncertainValue
  def union(other: UncertainValue): UncertainValue
  def to_expression[G](variable: Expr[G]): Expr[G]
  def ==(other: UncertainValue): UncertainBooleanValue
  def !=(other: UncertainValue): UncertainBooleanValue
  def t[G]: Type[G]
  def split: Option[Set[UncertainValue]]
}
case object UncertainValue {
  def uncertain_of(t: Type[_]): UncertainValue =
    t match {
      case _: IntType[_] => UncertainIntegerValue.uncertain()
      case _: TBool[_] => UncertainBooleanValue.uncertain()
    }
  def empty_of(t: Type[_]): UncertainValue =
    t match {
      case _: IntType[_] => UncertainIntegerValue.empty()
      case _: TBool[_] => UncertainBooleanValue.empty()
    }
}

case class UncertainBooleanValue(can_be_true: Boolean, can_be_false: Boolean)
    extends UncertainValue {
  override def can_be_equal(other: UncertainValue): Boolean =
    other match {
      case UncertainBooleanValue(t, f) => can_be_true && t || can_be_false && f
      case _ => false
    }

  override def can_be_unequal(other: UncertainValue): Boolean =
    other match {
      case UncertainBooleanValue(t, f) => can_be_true && f || can_be_false && t
      case _ => true
    }

  override def is_uncertain: Boolean = can_be_true && can_be_false

  override def is_impossible: Boolean = !can_be_true && !can_be_false

  override def is_subset_of(other: UncertainValue): Boolean =
    other match {
      case UncertainBooleanValue(t, f) =>
        (t || !can_be_true) && (f || !can_be_false)
      case _ => false
    }

  override def complement(): UncertainValue = !this

  override def intersection(other: UncertainValue): UncertainValue =
    other match {
      case UncertainBooleanValue(t, f) =>
        UncertainBooleanValue(can_be_true && t, can_be_false && f)
      case _ =>
        throw new IllegalArgumentException(
          "Trying to intersect boolean with a different type"
        )
    }

  override def union(other: UncertainValue): UncertainValue =
    other match {
      case UncertainBooleanValue(t, f) =>
        UncertainBooleanValue(can_be_true || t, can_be_false || f)
      case _ =>
        throw new IllegalArgumentException(
          "Trying to union boolean with a different type"
        )
    }

  override def to_expression[G](variable: Expr[G]): Expr[G] = {
    if (can_be_true && can_be_false)
      BooleanValue(value = true)(variable.o)
    else if (can_be_true)
      variable
    else if (can_be_false)
      Not(variable)(variable.o)
    else
      BooleanValue(value = false)(variable.o)
  }

  override def ==(other: UncertainValue): UncertainBooleanValue =
    other match {
      case b: UncertainBooleanValue => this == b
      case _ => UncertainBooleanValue.from(false)
    }

  override def !=(other: UncertainValue): UncertainBooleanValue =
    other match {
      case b: UncertainBooleanValue => this != b
      case _ => UncertainBooleanValue.from(true)
    }

  override def t[G]: Type[G] = TBool[G]()

  override def split: Option[Set[UncertainValue]] = {
    if (is_impossible)
      None
    else {
      var res: Set[UncertainValue] = Set.empty[UncertainValue]
      if (can_be_true)
        res += UncertainBooleanValue.from(true)
      if (can_be_false)
        res += UncertainBooleanValue.from(false)
      Some(res)
    }
  }

  def try_to_resolve(): Option[Boolean] = {
    if (can_be_true && !can_be_false)
      Some(true)
    else if (can_be_false && !can_be_true)
      Some(false)
    else
      None
  }

  def &&(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(
      can_be_true && other.can_be_true,
      can_be_false || other.can_be_false,
    )
  def ||(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(
      can_be_true || other.can_be_true,
      can_be_false && other.can_be_false,
    )
  def unary_! : UncertainBooleanValue =
    UncertainBooleanValue(can_be_false, can_be_true)
  def ^(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(
      can_be_true && other.can_be_false || can_be_false && other.can_be_true,
      can_be_true && other.can_be_true || can_be_false && other.can_be_false,
    )
  def ==(other: UncertainBooleanValue): UncertainBooleanValue =
    UncertainBooleanValue(
      can_be_true && other.can_be_true || can_be_false && other.can_be_false,
      can_be_true && other.can_be_false || can_be_false && other.can_be_true,
    )
  def !=(other: UncertainBooleanValue): UncertainBooleanValue = this ^ other
}
case object UncertainBooleanValue {
  def empty(): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true = false, can_be_false = false)
  def from(bool: Boolean): UncertainBooleanValue =
    UncertainBooleanValue(bool, !bool)
  def uncertain(): UncertainBooleanValue =
    UncertainBooleanValue(can_be_true = true, can_be_false = true)
}

case class UncertainIntegerValue(value: Interval) extends UncertainValue {
  override def can_be_equal(other: UncertainValue): Boolean =
    other match {
      case UncertainIntegerValue(v) => value.intersection(v).non_empty()
      case _ => false
    }

  override def can_be_unequal(other: UncertainValue): Boolean =
    other match {
      case UncertainIntegerValue(v) =>
        value.empty() || v.empty() ||
        (value.union(v).size() match {
          case Infinite() => true
          case Finite(size) => size != 1
        })
      case _ => true
    }

  override def is_uncertain: Boolean = value == UnboundedInterval

  override def is_impossible: Boolean = value.empty()

  override def is_subset_of(other: UncertainValue): Boolean =
    other match {
      case UncertainIntegerValue(v) => value.is_subset_of(v)
      case _ => false
    }

  override def complement(): UncertainValue =
    value match {
      case BoundedInterval(lower, upper) if lower == upper =>
        UncertainIntegerValue(value.complement())
      case _ => UncertainIntegerValue.uncertain()
    }

  override def intersection(other: UncertainValue): UncertainValue =
    other match {
      case UncertainIntegerValue(v) =>
        UncertainIntegerValue(value.intersection(v))
      case _ =>
        throw new IllegalArgumentException(
          "Trying to intersect integer with different type"
        )
    }

  override def union(other: UncertainValue): UncertainValue =
    other match {
      case UncertainIntegerValue(v) => UncertainIntegerValue(value.union(v))
      case _ =>
        throw new IllegalArgumentException(
          "Trying to union integer with different type"
        )
    }

  override def to_expression[G](variable: Expr[G]): Expr[G] =
    value.to_expression(variable)

  override def ==(other: UncertainValue): UncertainBooleanValue =
    other match {
      case i: UncertainIntegerValue => this == i
      case _ => UncertainBooleanValue.from(false)
    }

  override def !=(other: UncertainValue): UncertainBooleanValue =
    other match {
      case i: UncertainIntegerValue => this != i
      case _ => UncertainBooleanValue.from(true)
    }

  override def t[G]: Type[G] = TInt[G]()

  override def split: Option[Set[UncertainValue]] =
    value.values match {
      case None => None
      case Some(ints) => Some(ints.map(i => UncertainIntegerValue.single(i)))
    }

  def try_to_resolve(): Option[Int] = value.try_to_resolve()

  def below_eq(): UncertainIntegerValue =
    UncertainIntegerValue(value.below_max())
  def below(): UncertainIntegerValue =
    below_eq() + UncertainIntegerValue.single(-1)
  def above_eq(): UncertainIntegerValue =
    UncertainIntegerValue(value.above_min())
  def above(): UncertainIntegerValue =
    above_eq() + UncertainIntegerValue.single(1)

  def ==(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_equal(other), can_be_unequal(other))
  def !=(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(can_be_unequal(other), can_be_equal(other))
  def >=(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(
      value.below_max().intersection(other.value).non_empty(),
      value.above_min().intersection(other.value.below_max()).size() >=
        Finite(2),
    )
  def <=(other: UncertainIntegerValue): UncertainBooleanValue =
    UncertainBooleanValue(
      value.above_min().intersection(other.value).non_empty(),
      value.below_max().intersection(other.value.above_min()).size() >=
        Finite(2),
    )
  def >(other: UncertainIntegerValue): UncertainBooleanValue = !(this <= other)
  def <(other: UncertainIntegerValue): UncertainBooleanValue = !(this >= other)

  def unary_- : UncertainIntegerValue = UncertainIntegerValue(-value)
  def +(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value + other.value)
  def -(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value - other.value)
  def *(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue(value * other.value)
  def /(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue
      .uncertain() // TODO: UncertainIntegerValue(value / other.value)
  def %(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue
      .uncertain() // TODO: UncertainIntegerValue(value % other.value)
  def pow(other: UncertainIntegerValue): UncertainIntegerValue =
    UncertainIntegerValue
      .uncertain() // TODO: UncertainIntegerValue(value.pow(other.value))
}
case object UncertainIntegerValue {
  def empty(): UncertainIntegerValue = UncertainIntegerValue(EmptyInterval)
  def uncertain(): UncertainIntegerValue =
    UncertainIntegerValue(UnboundedInterval)
  def above(int: Int): UncertainIntegerValue =
    UncertainIntegerValue(LowerBoundedInterval(int))
  def single(int: Int): UncertainIntegerValue =
    UncertainIntegerValue(BoundedInterval(int, int))
}

case class UncertainSequence(
    len: UncertainIntegerValue,
    values: Seq[(UncertainIntegerValue, UncertainValue)],
    t: Type[_],
) {
  def ==(other: UncertainSequence): UncertainBooleanValue = {
    if (len.intersection(other.len).is_impossible)
      return UncertainBooleanValue.from(false)
    if (
      values.exists(t1 =>
        t1._1.try_to_resolve().nonEmpty && other.values.exists(t2 =>
          !t2._1.intersection(t1._1).is_impossible &&
            t2._2.can_be_unequal(t1._2)
        )
      )
    )
      return UncertainBooleanValue.from(false)

    val length: Option[Int] = len.try_to_resolve()
    val other_length: Option[Int] = other.len.try_to_resolve()
    if (
      length.nonEmpty && other_length.nonEmpty && length.get == other_length.get
    ) {
      for (i <- Range(0, length.get)) {
        val values_index: Int = values
          .indexWhere(t => t._1.try_to_resolve().getOrElse(-1) == i)
        val other_values_index: Int = other.values
          .indexWhere(t => t._1.try_to_resolve().getOrElse(-1) == i)
        if (values_index >= 0 && other_values_index >= 0) {
          val value = values(values_index)._2
          val other_value = other.values(other_values_index)._2
          if (!value.can_be_unequal(other_value))
            return UncertainBooleanValue.uncertain()
        } else
          return UncertainBooleanValue.uncertain()
      }
    } else
      return UncertainBooleanValue.uncertain()
    // Only if the lengths are exactly the same and perfectly certain,
    // and every value in between is defined in both sequences and perfectly certain and equal,
    // only then are two uncertain sequences definitely equal
    UncertainBooleanValue.from(true)
  }

  def union(other: UncertainSequence): UncertainSequence = {
    if (t != other.t)
      throw new IllegalArgumentException(
        "Unioning sequences of different types"
      )
    UncertainSequence(
      len.union(other.len).asInstanceOf[UncertainIntegerValue],
      combine_values(values, other.values),
      t,
    )
  }

  def concat(other: UncertainSequence): UncertainSequence =
    UncertainSequence(len + other.len, values ++ shift(other.values, len), t)

  def prepend(value: UncertainValue): UncertainSequence =
    UncertainSequence(
      len + UncertainIntegerValue.single(1),
      (UncertainIntegerValue.single(0) -> value) +:
        shift(values, UncertainIntegerValue.single(1)),
      t,
    )

  def updated(
      index: UncertainIntegerValue,
      value: UncertainValue,
  ): UncertainSequence =
    UncertainSequence(
      len,
      values.filter(e => !e._1.can_be_equal(index)) :+ index -> value,
      t,
    )

  def remove(index: UncertainIntegerValue): UncertainSequence = {
    val (before, after) = values.filter(e => !e._1.can_be_equal(index))
      .partition(e => (e._1 < index).can_be_true)
    UncertainSequence(
      len - UncertainIntegerValue.single(1),
      before ++ shift(after, UncertainIntegerValue.single(-1)),
      t,
    )
  }

  def take(num: UncertainIntegerValue): UncertainSequence =
    UncertainSequence(
      num,
      values.filter(t => t._1.<(num).can_be_true).map(t =>
        t._1.intersection(num.below()).asInstanceOf[UncertainIntegerValue] ->
          t._2
      ),
      t,
    )

  def drop(num: UncertainIntegerValue): UncertainSequence = {
    val red: UncertainIntegerValue = num.intersection(len.below())
      .asInstanceOf[UncertainIntegerValue]
    val remaining: Seq[(UncertainIntegerValue, UncertainValue)] = values
      .filter(t => t._1.>=(red).can_be_true).map(t =>
        t._1.intersection(red.above_eq()).asInstanceOf[UncertainIntegerValue] ->
          t._2
      )
    UncertainSequence(
      if (red.value.non_empty())
        len - red
      else
        UncertainIntegerValue.single(0),
      shift(remaining, -red),
      t,
    )
  }

  def slice(
      lower: UncertainIntegerValue,
      upper: UncertainIntegerValue,
  ): UncertainSequence = take(upper).drop(lower)

  def get(index: Int): UncertainValue = {
    if (index < 0)
      throw new IllegalArgumentException(
        s"Trying to access negative index $index"
      )
    val i = values.indexWhere(t => t._1.try_to_resolve().getOrElse(-1) == index)
    if (i >= 0)
      values(i)._2
    else
      UncertainValue.uncertain_of(t)
  }

  private def combine_values(
      v1: Seq[(UncertainIntegerValue, UncertainValue)],
      v2: Seq[(UncertainIntegerValue, UncertainValue)],
  ): Seq[(UncertainIntegerValue, UncertainValue)] = {
    var res: Seq[(UncertainIntegerValue, UncertainValue)] = Seq()
    for (v <- v1) {
      for (comp <- v2) {
        if (v._1.is_subset_of(comp._1) && v._2.is_subset_of(comp._2))
          res :+= comp
        else if (comp._1.is_subset_of(v._1) && comp._2.is_subset_of(v._2))
          res :+= v
      }
    }
    res.distinct
  }

  private def shift(
      sequence: Seq[(UncertainIntegerValue, UncertainValue)],
      value: UncertainIntegerValue,
  ): Seq[(UncertainIntegerValue, UncertainValue)] =
    sequence.map(e => (e._1 + value, e._2))
}
case object UncertainSequence {
  def uncertain(t: Type[_]): UncertainSequence =
    UncertainSequence(UncertainIntegerValue.above(0), Seq(), t)
  def empty(t: Type[_]): UncertainSequence =
    UncertainSequence(UncertainIntegerValue.empty(), Seq(), t)
}
