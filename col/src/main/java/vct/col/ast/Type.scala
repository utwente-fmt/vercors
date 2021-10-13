package vct.col.ast

import vct.col.resolve.Referrable
import vct.result.VerificationResult.Unreachable

import scala.reflect.ClassTag

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

  def asSeq: Option[TSeq] = optMatch(mimics) { case seq: TSeq => seq }
  def asSet: Option[TSet] = optMatch(mimics) { case set: TSet => set }
  def asBag: Option[TBag] = optMatch(mimics) { case bag: TBag => bag }
  def asPointer: Option[TPointer] = optMatch(mimics) { case ptr: TPointer => ptr }
  def asArray: Option[TArray] = optMatch(mimics) { case arr: TArray => arr }
  def asOption: Option[TOption] = optMatch(mimics) { case opt: TOption => opt }
  def asMap: Option[TMap] = optMatch(mimics) { case map: TMap => map }
  def asTuple: Option[TTuple] = optMatch(mimics) { case tup: TTuple => tup }
  /*def asVector: Option[TVector] = optMatch(mimics) { case vec: TVector => vec }*/
  def asMatrix: Option[TMatrix] = optMatch(mimics) { case mat: TMatrix => mat }
  def asModel: Option[TModel] = optMatch(mimics) { case model: TModel => model }
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
  )

  def leastCommonSuperType(ts: Seq[Type]): Type =
    ts.foldLeft[Type](TNothing())(leastCommonSuperType)

  def leastCommonSuperType(left: Type, right: Type): Type = (left, right) match {
    // The result for NotAValue should not matter, since they should be filtered out by LangSpecificToCol.
    case (TNotAValue(), _) => TNotAValue()
    case (_, TNotAValue()) => TNotAValue()

    // Any other types are below Any, so we can safely default to that from here.
    // First the simple case where either type is a supertype of the other
    case (left, right) if left.superTypeOf(right) => left
    case (left, right) if right.superTypeOf(left) => right

    // Covariant types can apply the least common supertype in their type argument
    case (TOption(left), TOption(right)) =>
      TOption(leastCommonSuperType(left, right))
    case (TTuple(left), TTuple(right)) if left.size == right.size =>
      TTuple(left.zip(right).map { case (l, r) => leastCommonSuperType(l, r) })
    case (TSeq(left), TSeq(right)) =>
      TSeq(leastCommonSuperType(left, right))
    case (TSet(left), TSet(right)) =>
      TSet(leastCommonSuperType(left, right))
    case (TBag(left), TBag(right)) =>
      TBag(leastCommonSuperType(left, right))
    case (TMatrix(left), TMatrix(right)) =>
      TMatrix(leastCommonSuperType(left, right))
    case (TMap(leftK, leftV), TMap(rightK, rightV)) =>
      // Map is not covariant in the key, so if the keys are inequal the best we can do is Any
      if(leftK == rightK) TMap(leftK, leastCommonSuperType(leftV, rightV))
      else TAny()
    case (TType(left), TType(right)) =>
      TType(leastCommonSuperType(left, right))

    case (TClass(left), TClass(right)) =>
      val leftArrows = left.decl.transSupportArrows
      val rightArrows = right.decl.transSupportArrows
      // Shared support are classes where there is an incoming left-arrow and right-arrow
      // If left supports right (or vice-versa), there would be a problem, since right will not have a self-arrow
      // However, this is caught by the simple sub-typing relation above already.
      val shared = leftArrows.collect { case (_, sup) if rightArrows.exists { case (_, rsup) => rsup == sup } => sup }
      // We are not interested in types that occur above shared types
      val nonBottom = leftArrows.intersect(rightArrows).map { case (sub, sup) => sup }
      val classes = (shared.toSet -- nonBottom.toSet).toSeq
      classes match {
        case Nil => TAny()
        case Seq(t) => TClass(t.ref)
        case other => JavaTUnion(other.map(cls => TClass(cls.ref)))
      }

    // TODO similar stuff for JavaClass

    case (JavaTUnion(left), JavaTUnion(right)) => JavaTUnion((left ++ right).distinct)
    case (JavaTUnion(left), right) => JavaTUnion((left :+ right).distinct)
    case (left, JavaTUnion(right)) => JavaTUnion((left +: right).distinct)

    // Unrelated types below rational are simply a rational
    case (left, right) if TRational().superTypeOf(left) && TRational().superTypeOf(right) =>
      TRational()

    case (_, _) => TAny()
  }

  def isComparable(left: Type, right: Type): Boolean =
    /* left == right || ROOT_TYPES.map(_.superTypeOf(left)) == ROOT_TYPES.map(_.superTypeOf(right)) */
    true // TODO is this bad?

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
sealed abstract class CovariantType[T <: CovariantType[_]](subTypes: => Seq[Type])(implicit val tag: ClassTag[T]) extends Type {
  private def types: Seq[Type] = subTypes

  override def superTypeOfImpl(other: Type): Boolean = {
    other match {
      case other: T =>
        types.size == other.types.size &&
          types.zip(other.types).forall {
            case (t, otherT) => t.superTypeOf(otherT)
          }
      case _ => false
    }
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

case class TNothing()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = other == TNothing()
  override def subTypeOfImpl(other: Type): Boolean = other != TNotAValue()
}

case class TVoid()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TNull()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = other == TNull()

  override def subTypeOfImpl(other: Type): Boolean = other match {
    case TPointer(_) => true
    case TClass(_) => true
    case JavaTClass(_) => true
    case TArray(_) => true
    case TRef() => true
    case _ => false
  }
}
case class TBool()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TResource()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean =
    other == TResource() || other == TBool()
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
case class TOption(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TOption](Seq(element))
case class TTuple(elements: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TTuple](elements)
case class TSeq(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TSeq](Seq(element)) with CollectionType
case class TSet(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TSet](Seq(element)) with CollectionType
case class TBag(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TBag](Seq(element)) with CollectionType
/* case class TVector(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TVector](Seq(element)) with CollectionType */
case class TMatrix(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TMatrix](Seq(element))
case class TArray(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TPointer(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TMap(key: Type, value: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override protected def superTypeOfImpl(other: Type): Boolean = other match {
    case TMap(k, other) =>
      // key occurs in contra- and covariant positions (e.g. MapGet and MapItemSet)
      // value currently appears only in covariant positions (i.e. we don't have MapContains(value))
      key == k && value.superTypeOf(other)
    case _ => false
  }
}
case class TProcess()(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TModel(model: Ref[Model])(implicit val o: Origin = DiagnosticOrigin) extends LeafType
case class TClass(cls: Ref[Class])(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def superTypeOfImpl(other: Type): Boolean = other == TClass(cls) // FIXME

  def transSupportArrows: Seq[(Class, Class)] = cls.decl.transSupportArrows
}
// PB: Potentially axiomatic datatypes could be covariant in its type arguments, but that will probably be a huge mess to
// translate into silver.
case class TAxiomatic(adt: Ref[AxiomaticDataType], args: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends LeafType

// the type type is covariant in its type (yes)
case class TType(t: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TType](Seq(t))