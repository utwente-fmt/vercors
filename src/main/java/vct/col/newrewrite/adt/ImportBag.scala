package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.ref.Ref
import vct.col.rewrite.Generation

case object ImportBag extends ImportADTBuilder("bag")

case class ImportBag[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  private lazy val bagFile = parse("bag")

  private lazy val bagAdt = find[AxiomaticDataType[Post]](bagFile, "bag")
  private lazy val bagMin = find[ADTFunction[Post]](bagAdt, "bag_min")
  private lazy val bagClip = find[ADTFunction[Post]](bagAdt, "bag_clip")
  private lazy val bagSize = find[ADTFunction[Post]](bagAdt, "bag_size")
  private lazy val bagCount = find[ADTFunction[Post]](bagAdt, "bag_count")
  private lazy val bagEmpty = find[ADTFunction[Post]](bagAdt, "bag_empty")
  private lazy val bagSingleton = find[ADTFunction[Post]](bagAdt, "bag_singleton")
  private lazy val bagUnionOne = find[ADTFunction[Post]](bagAdt, "bag_union_one")
  private lazy val bagUnion = find[ADTFunction[Post]](bagAdt, "bag_union")
  private lazy val bagIntersection = find[ADTFunction[Post]](bagAdt, "bag_intersection")
  private lazy val bagDifference = find[ADTFunction[Post]](bagAdt, "bag_difference")
  private lazy val bagSubbag = find[ADTFunction[Post]](bagAdt, "bag_subbag")
  private lazy val bagEqual = find[ADTFunction[Post]](bagAdt, "bag_equal")
  private lazy val bagSkolem = find[ADTFunction[Post]](bagAdt, "bag_skolem")
  private lazy val bagDisjoint = find[ADTFunction[Post]](bagAdt, "bag_disjoint")
  private lazy val bagDisjointSkolem = find[ADTFunction[Post]](bagAdt, "bag_disjoint_skolem")

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TBag(inner) => TAxiomatic[Post](bagAdt.ref, Seq(dispatch(inner)))(t.o)
    case other => rewriteDefault(other)
  }

  def typeArgs(xs: Expr[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    typeArgs(xs.t.asBag.get.element)

  def typeArgs(t: Type[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    Some((bagAdt.ref, Seq(dispatch(t))))

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Size(xs) if xs.t.asBag.nonEmpty =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = bagSize.ref,
        args = Seq(dispatch(xs)),
      )(e.o)

    case BagMemberCount(x, xs) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = bagCount.ref,
        args = Seq(dispatch(xs), dispatch(x)),
      )(e.o)

    case LiteralBag(element, Nil) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(element),
        ref = bagEmpty.ref,
        args = Nil,
      )(e.o)

    case LiteralBag(element, x :: xs) =>
      xs.foldLeft(
        ADTFunctionInvocation[Post](
          typeArgs = typeArgs(element),
          ref = bagSingleton.ref,
          args = Seq(dispatch(x)),
        )(e.o)
      ) {
        case (init, next) =>
          ADTFunctionInvocation[Post](
            typeArgs = typeArgs(element),
            ref = bagUnionOne.ref,
            args = Seq(init, dispatch(next)),
          )(e.o)
      }

    case union @ BagAdd(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(union.t.element),
        ref = bagUnion.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case int @ BagLargestCommon(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(int.t.element),
        ref = bagIntersection.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case min @ BagMinus(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(min.t.element),
        ref = bagDifference.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case sub @ SubBagEq(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(sub.comparisonType.asBag.get),
        ref = bagSubbag.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case eq @ Eq(xs, ys) if eq.comparisonType.asBag.nonEmpty =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(eq.comparisonType.asBag.get.element),
        ref = bagEqual.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case neq @ Neq(xs, ys) if neq.comparisonType.asBag.nonEmpty =>
      Not(ADTFunctionInvocation[Post](
        typeArgs = typeArgs(neq.comparisonType.asBag.get.element),
        ref = bagEqual.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o))(e.o)

    case other => rewriteDefault(other)
  }
}
