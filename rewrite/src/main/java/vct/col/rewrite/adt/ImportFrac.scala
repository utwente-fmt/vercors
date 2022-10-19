package vct.col.rewrite.adt

import vct.col.ast.{ADTFunction, ADTFunctionInvocation, AxiomaticDataType, CoerceBoundIntFrac, CoerceBoundIntZFrac, CoerceFracZFrac, CoerceRatZFrac, CoerceZFracFrac, CoerceZFracRat, Coercion, CoercionSequence, Expr, Function, FunctionInvocation, NoPerm, Select, TAxiomatic, TFraction, TZFraction, Type, WritePerm}
import vct.col.origin._
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers._

case object ImportFrac extends ImportADTBuilder("frac") {
  case class RatZFracPreconditionFailed(inner: Blame[CoerceRatZFracFailed], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceRatZFracFailed(expr))
  }

  case class RatFracPreconditionFailed(inner: Blame[CoerceRatFracFailed], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceRatFracFailed(expr))
  }

  case class ZFracFracPreconditionFailed(inner: Blame[CoerceZFracFracFailed], expr: Expr[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(CoerceZFracFracFailed(expr))
  }
}

case class ImportFrac[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  import ImportFrac._

  private lazy val fracFile = parse("frac")
  private lazy val zfracFile = parse("zfrac")

  private lazy val fracAdt = find[AxiomaticDataType[Post]](fracFile, "frac")
  private lazy val fracVal = find[ADTFunction[Post]](fracAdt, "frac_val")
  private lazy val fracNew = find[Function[Post]](fracFile, "new_frac")
  private lazy val zfracAdt = find[AxiomaticDataType[Post]](zfracFile, "zfrac")
  private lazy val zfracVal = find[ADTFunction[Post]](zfracAdt, "zfrac_val")
  private lazy val zfracNew = find[Function[Post]](zfracFile, "new_zfrac")

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceZFracRat() =>
      ADTFunctionInvocation[Post](Some((zfracAdt.ref, Nil)), zfracVal.ref, Seq(e))
    case CoercionSequence(Seq(CoerceFracZFrac(), CoerceZFracRat())) =>
      ADTFunctionInvocation(Some((fracAdt.ref, Nil)), fracVal.ref, Seq(e))
    case CoerceFracZFrac() =>
      val rat = ADTFunctionInvocation[Post](Some((fracAdt.ref, Nil)), fracVal.ref, Seq(e))
      FunctionInvocation[Post](zfracNew.ref, Seq(rat), Nil, Nil, Nil)(PanicBlame("a frac always fits in a zfrac."))

    case CoerceRatZFrac() =>
      FunctionInvocation[Post](zfracNew.ref, Seq(e), Nil, Nil, Nil)(NoContext(RatZFracPreconditionFailed(globalBlame.top, e)))
    case CoercionSequence(Seq(CoerceRatZFrac(), CoerceZFracFrac())) =>
      FunctionInvocation[Post](fracNew.ref, Seq(e), Nil, Nil, Nil)(NoContext(RatZFracPreconditionFailed(globalBlame.top, e)))
    case CoerceZFracFrac() =>
      val rat = ADTFunctionInvocation[Post](Some((zfracAdt.ref, Nil)), zfracVal.ref, Seq(e))
      FunctionInvocation[Post](fracNew.ref, Seq(rat), Nil, Nil, Nil)(NoContext(ZFracFracPreconditionFailed(globalBlame.top, e)))

    case CoerceBoundIntFrac() =>
      FunctionInvocation[Post](fracNew.ref, Seq(WritePerm()), Nil, Nil, Nil)(PanicBlame("The constant 1 always fits in a frac."))
    case CoerceBoundIntZFrac(_) =>
      FunctionInvocation[Post](zfracNew.ref, Seq(Select(e === const(0), NoPerm(), WritePerm())), Nil, Nil, Nil)(PanicBlame("The constants 0 and 1 always fit in a zfrac."))

    case other => super.applyCoercion(e, other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TFraction() => TAxiomatic[Post](fracAdt.ref, Nil)(t.o)
    case TZFraction() => TAxiomatic(zfracAdt.ref, Nil)
    case other => rewriteDefault(other)
  }
}
