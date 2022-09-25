package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewritten}

case object ImportSet extends ImportADTBuilder("set")

case class ImportSet[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  private lazy val setFile = parse("set")

  private lazy val setAdt = find[AxiomaticDataType[Post]](setFile, "set")
  private lazy val setSize = find[ADTFunction[Post]](setAdt, "set_size")
  private lazy val setIn = find[ADTFunction[Post]](setAdt, "set_in")
  private lazy val setEmpty = find[ADTFunction[Post]](setAdt, "set_empty")
  private lazy val setSingleton = find[ADTFunction[Post]](setAdt, "set_singleton")
  private lazy val setUnionOne = find[ADTFunction[Post]](setAdt, "set_union_one")
  private lazy val setUnion = find[ADTFunction[Post]](setAdt, "set_union")
  private lazy val setIntersection = find[ADTFunction[Post]](setAdt, "set_intersection")
  private lazy val setDifference = find[ADTFunction[Post]](setAdt, "set_difference")
  private lazy val setSubset = find[ADTFunction[Post]](setAdt, "set_subset")
  private lazy val setEqual = find[ADTFunction[Post]](setAdt, "set_equal")
  private lazy val setSkolem = find[ADTFunction[Post]](setAdt, "set_skolem")

  override def dispatch(t: Type[Pre]): Type[Rewritten[Pre]] = t match {
    case TSet(inner) => TAxiomatic[Post](setAdt.ref, Seq(dispatch(inner)))(t.o)
    case other => rewriteDefault(other)
  }

  def typeArgs(xs: Expr[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    typeArgs(xs.t.asSet.get.element)

  def typeArgs(t: Type[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    Some((setAdt.ref, Seq(dispatch(t))))

  override def postCoerce(e: Expr[Pre]): Expr[Post] = e match {
    case Size(xs) if xs.t.asSet.nonEmpty =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = setSize.ref,
        args = Seq(dispatch(xs)),
      )(e.o)

    case SetMember(x, xs) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = setIn.ref,
        args = Seq(dispatch(xs), dispatch(x)),
      )(e.o)

    case LiteralSet(element, Nil) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(element),
        ref = setEmpty.ref,
        args = Nil,
      )(e.o)

    case LiteralSet(element, x :: xs) =>
      xs.foldLeft(
        ADTFunctionInvocation[Post](
          typeArgs = typeArgs(element),
          ref = setSingleton.ref,
          args = Seq(dispatch(x)),
        )(e.o)
      ) {
        case (init, next) =>
          ADTFunctionInvocation[Post](
            typeArgs = typeArgs(element),
            ref = setUnionOne.ref,
            args = Seq(init, dispatch(next)),
          )(e.o)
      }

    case union @ SetUnion(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(union.t.element),
        ref = setUnion.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case int @ SetIntersection(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(int.t.element),
        ref = setIntersection.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case diff @ SetMinus(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(diff.t.element),
        ref = setDifference.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case SubSetEq(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs.t.asSet.get.element),
        ref = setSubset.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case eq @ Eq(xs, ys) if eq.comparisonType.asSet.nonEmpty =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(eq.comparisonType.asSeq.get.element),
        ref = setEqual.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(eq.o)

    case neq @ Neq(xs, ys) if neq.comparisonType.asSet.nonEmpty =>
      Not(ADTFunctionInvocation[Post](
        typeArgs = typeArgs(neq.comparisonType.asSeq.get.element),
        ref = setEqual.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(neq.o))(neq.o)

    case other => rewriteDefault(other)
  }
}
