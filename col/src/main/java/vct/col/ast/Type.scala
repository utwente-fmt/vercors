package vct.col.ast

import vct.col.resolve.Referrable
import AstBuildHelpers._

sealed trait Type extends NodeFamily {
  def superTypeOf(other: Type): Boolean =
    Coercion.getCoercion(other.mimics, mimics).isDefined

  def mimics: Type = this

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

  def particularize(substitutions: Map[Variable, Type]): Type = {
    case object Particularize extends Rewriter {
      override def dispatch(t: Type): Type = t.mimics match {
        case TVar(Ref(v)) => substitutions(v)
        case _ => t match {
          case t: PVLNamedType => t
          case t: JavaTClass => t
          case TModel(ref) => TModel(ref)
          case TClass(ref) => TClass(ref)
          case TAxiomatic(ref, args) => TAxiomatic(ref, args.map(dispatch))
          case other => rewriteDefault(other)
        }
      }
    }
    Particularize.dispatch(this)
  }
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
        case other => TUnion(other.map(cls => TClass(cls.ref)))
      }

    // TODO similar stuff for JavaClass

    case (TUnion(left), TUnion(right)) => TUnion((left ++ right).distinct)
    case (TUnion(left), right) => TUnion((left :+ right).distinct)
    case (left, TUnion(right)) => TUnion((left +: right).distinct)

    case (TBoundedInt(leftGte, leftLt), TBoundedInt(rightGte, rightLt)) =>
      TBoundedInt(leftGte.min(rightGte), leftLt.max(rightLt))

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

// Immutable collection with a defined size
sealed trait CollectionType extends Type

object TNotAValue {
  def apply(decl: Referrable): TNotAValue = {
    val res = TNotAValue()
    res.decl = Some(decl)
    res
  }
}

case class TNotAValue()(implicit val o: Origin = DiagnosticOrigin) extends Type {
  var decl: Option[Referrable] = None
}

case class TAny()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TNothing()(implicit val o: Origin = DiagnosticOrigin) extends Type

case class TUnion(types: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def mimics: Type =
    if(types.size == 1) types.head.mimics
    else TUnion(types.map(_.mimics))

//  override def subTypeOfImpl(other: Type): Boolean =
//    types.forall(other.superTypeOf)
//
//  override def superTypeOfImpl(other: Type): Boolean =
//    types.exists(_.superTypeOf(other))
}

case class TVoid()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TNull()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TBool()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TResource()(implicit val o: Origin = DiagnosticOrigin) extends Type

case class TInt()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TBoundedInt(gte: BigInt, lt: BigInt)(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TFloat()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TRational()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TFraction()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TZFraction()(implicit val o: Origin = DiagnosticOrigin) extends Type

case class TChar()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TString()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TRef()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TOption(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TTuple(elements: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TSeq(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CollectionType
case class TSet(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CollectionType
case class TBag(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CollectionType
/* case class TVector(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CovariantType[TVector](Seq(element)) with CollectionType */
case class TMatrix(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TArray(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TPointer(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TMap(key: Type, value: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TProcess()(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TModel(model: Ref[Model])(implicit val o: Origin = DiagnosticOrigin) extends Type
case class TClass(cls: Ref[Class])(implicit val o: Origin = DiagnosticOrigin) extends Type {
  def transSupportArrows: Seq[(Class, Class)] = cls.decl.transSupportArrows
}
case class TAxiomatic(adt: Ref[AxiomaticDataType], args: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends Type

case class TType(t: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type

case class TVar(ref: Ref[Variable])(implicit val o: Origin = DiagnosticOrigin) extends Type {
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref) ++
      (if(TType(TAny()).superTypeOf(ref.decl.t)) Nil
      else Seq(GenericTypeError(this, TType(TAny()))))
}