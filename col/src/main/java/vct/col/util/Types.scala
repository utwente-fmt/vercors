package vct.col.util

import vct.col.ast._

object Types {
  def leastCommonSuperType(ts: Seq[Type]): Type =
    ts.foldLeft[Type](TNothing())(leastCommonSuperType)

  def leastCommonSuperType(left: Type, right: Type): Type = (left, right) match {
    // The result for NotAValue should not matter, since they should be filtered out by LangSpecificToCol.
    case (_: TNotAValue, _) => new TNotAValue(None)
    case (_, _: TNotAValue) => new TNotAValue(None)

    // Any other types are below Any, so we can safely default to that from here.
    // First the simple case where either type is a supertype of the other
    case (left, right) if left.superTypeOf(right) => left
    case (left, right) if right.superTypeOf(left) => right

    // Covariant types can apply the least common supertype in their type argument
    case (TOption(left), TOption(right)) =>
      TOption(leastCommonSuperType(left, right))
    case (TEither(leftLeft, leftRight), TEither(rightLeft, rightRight)) =>
      TEither(
        leastCommonSuperType(leftLeft, rightLeft),
        leastCommonSuperType(leftRight, rightRight),
      )
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
}