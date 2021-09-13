package vct.col.ast

import vct.col.resolve.Referrable

sealed trait Type extends NodeFamily {
  def superTypeOf(other: Type): Boolean =
    mimics.superTypeOfImpl(other.mimics) ||
      other.mimics.subTypeOfImpl(mimics)

  def mimics: Type = this

  protected def superTypeOfImpl(other: Type): Boolean
  protected def subTypeOfImpl(other: Type): Boolean = false

  override def check(context: CheckContext): Seq[CheckError] = Nil

  private def optMatch[In, Out](arg: In)(matchFunc: PartialFunction[In, Out]): Option[Out] =
    matchFunc.lift(arg)

  def asSeq: Option[TSeq] = optMatch(this) { case seq: TSeq => seq }
  def asSet: Option[TSet] = optMatch(this) { case set: TSet => set }
  def asBag: Option[TBag] = optMatch(this) { case bag: TBag => bag }
  def asPointer: Option[TPointer] = optMatch(this) { case ptr: TPointer => ptr }
  def asArray: Option[TArray] = optMatch(this) { case arr: TArray => arr }
  def asOption: Option[TOption] = optMatch(this) { case opt: TOption => opt }
  def asMap: Option[TMap] = optMatch(this) { case map: TMap => map }
  def asTuple: Option[TTuple] = optMatch(this) { case tup: TTuple => tup }
  def asModel: Option[TModel] = optMatch(this) { case model: TModel => model }
}

trait ExtraType extends Type

object Type {
  // All types under a root type are comparable
  val ROOT_TYPES: Seq[Type] = Seq(
    TVoid(),
    TResource(),
    TRational(),
    TString(),
    TRef(),
    TProcess(),
    TClass.OBJECT,
  )

  def leastCommonSuperType(left: Type, right: Type): Type =
    left

  def greatestCommonSubType(left: Type, right: Type): Type =
    left

  def isComparable(left: Type, right: Type): Boolean =
    left == right || ROOT_TYPES.map(_.superTypeOf(left)) == ROOT_TYPES.map(_.superTypeOf(right))

  def checkComparable(left: Expr, right: Expr): Seq[CheckError] =
    if(isComparable(left.t, right.t)) Seq()
    else Seq(IncomparableTypes(left, right))
}

sealed trait LeafType extends Type {
  override def superTypeOfImpl(other: Type): Boolean =
    other == this
}

// A Seq[Cat] is a Seq[Animal] because a Cat is an Animal. Seq is then covariant in its element type. This is nice to
// have, and works because most of our types are immutable ("query-only").
sealed abstract class CovariantType(subTypes: => Seq[Type]) extends Type {
  private def types: Seq[Type] = subTypes

  override def superTypeOfImpl(other: Type): Boolean =
    other match {
      case other: this.type =>
        types.size == other.types.size &&
          types.zip(other.types).forall {
            case (t, otherT) => t.superTypeOf(otherT)
          }
      case _ => false
    }
}

// Immutable collection with a defined size
sealed trait CollectionType

object TNotAValue {
  def apply(decl: Referrable): TNotAValue = {
    val res = TNotAValue()
    res.decl = Some(decl)
    res
  }
}

case class TNotAValue()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  var decl: Option[Referrable] = None
  override protected def superTypeOfImpl(other: Type): Boolean = false
}

case class TAny()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = other match {
    case TNotAValue() => false
    case _ => true
  }
}

case class TVoid()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TNull()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = ???
}
case class TBool()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TResource()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean =
    Set[Type](TResource(), TBool()).contains(other)
}

case class TInt()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override protected def superTypeOfImpl(other: Type): Boolean = other match {
    case TInt() | TBoundedInt(_, _) => true
    case _ => false
  }
}
case class TBoundedInt(gte: BigInt, lt: BigInt)(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = other match {
    case TBoundedInt(otherGte, otherLt) => gte <= otherGte && otherLt <= lt
    case _ => false
  }
}
case class TFloat()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TRational()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = other match {
    case TRational() | TInt() | TFraction() | TZFraction() | TBoundedInt(_, _) => true
    case _ => false
  }
}
case class TFraction()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override protected def superTypeOfImpl(other: Type): Boolean = other match {
    case TFraction() => true
    case TBoundedInt(gte, lt) if gte > 0 && lt <= 2 => true // At most [1, 2) ∩ ℤ = {1}
    case _ => false
  }
}
case class TZFraction()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = other match {
    case TFraction() | TZFraction() => true
    case TBoundedInt(gte, lt) if gte >= 0 && lt <= 2 => true // At most [0, 2) ∩ ℤ = {0, 1}
  }
}

case class TChar()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TString()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TRef()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TOption(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType(Seq(element))
case class TTuple(elements: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends CovariantType(elements)
case class TSeq(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType(Seq(element)) with CollectionType
case class TSet(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType(Seq(element)) with CollectionType
case class TBag(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType(Seq(element)) with CollectionType
case class TArray(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TPointer(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TMap(key: Type, value: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType(Seq(key, value)) with CollectionType
case class TProcess()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
object TClass {
//  val OBJECT: TClass = TClass(new DirectRef[Class](null))(null) // FIXME
//  val THROWABLE: TClass = TClass(new DirectRef[Class](null))(null) // FIXME
//  val RUNNABLE: TClass = TClass(new DirectRef[Class](null))(null) // FIXME
  val OBJECT: TInt = TInt()(DiagnosticOrigin)
  val THROWABLE: TInt = TInt()(DiagnosticOrigin)
  val RUNNABLE: TInt = TInt()(DiagnosticOrigin)
}
case class TModel(model: Ref[Model])(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TClass(cls: Ref[Class])(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = false // FIXME
}
// PB: Potentially axiomatic datatypes could be covariant in its type arguments, but that will probably be a huge mess to
// translate into silver.
case class TAxiomatic(adt: Ref[AxiomaticDataType], args: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends LeafType

// the type type is covariant in its type (yes)
case class TType(t: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType(Seq(t))