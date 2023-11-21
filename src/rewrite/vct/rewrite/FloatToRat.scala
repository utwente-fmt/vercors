package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.`type`.typeclass.TFloats
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.lang.PVL
import vct.col.rewrite.FloatToRat.CastFuncOrigin
import vct.col.rewrite.error.ExtraNode
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object FloatToRat extends RewriterBuilder {
  override def key: String = "floatToRat"
  override def desc: String = "Converts floating point values and types into rationals, disregarding precision, nan, and other practical concerns"

  private def CastFuncOrigin(preferredName: String): Origin = Origin(
    Seq(
      PreferredName(Seq(preferredName)),
      LabelContext("float to rat"),
    )
  )
}

case class FloatToRat[Pre <: Generation]() extends Rewriter[Pre] {
  def name(t: TFloat[_]) = t match {
    case t if t == PVL.float64 => "f64"
    case t if t == PVL.float32 => "f32"
    case TFloat(e, m) => s"f${e}_$m"
  }

  private val nonDetFloatOrigin: Origin = Origin(Seq(LabelContext("float to rational conversion")))

  val nonDetFloat: mutable.Map[Unit, Function[Post]] = mutable.Map()

  def getNonDetFloat(): Expr[Post] = {
    val nondetFunc = nonDetFloat.getOrElseUpdate((), makeNondetFloatFunc())
    FunctionInvocation[Post](nondetFunc.ref, Seq(), Nil, Nil, Nil)(TrueSatisfiable)(nonDetFloatOrigin)
  }

  def makeNondetFloatFunc(): Function[Post] = {
    globalDeclarations.declare(withResult((result: Result[Post]) => function[Post](
      blame = AbstractApplicable,
      contractBlame = TrueSatisfiable,
      returnType = TRational(),
    )(nonDetFloatOrigin.where(name= "nonDetFloat")))(nonDetFloatOrigin))
  }
  
  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case CastFloat(e, t) if e.t == t => dispatch(e)
    case CastFloat(e, t: TFloat[Pre]) if e.t.isInstanceOf[TFloat[Pre]] => dispatch(e)
    case c@CastFloat(e, t: TFloat[Pre]) if CoercionUtils.getCoercion(e.t, TInt()).isDefined =>
      implicit val o: Origin = c.o
      dispatch(e) /:/ const(1)
    case c@CastFloat(e, t: TInt[Pre]) if e.t.isInstanceOf[TFloat[Pre]] =>
      SmtlibToInt[Post](dispatch(e))(CastFuncOrigin("to_int"))
    case CastFloat(_, _) => ???
    case f @ FloatValue(num, _) =>
      implicit val o: Origin = f.o
      var numerator = num
      var denominator = BigInt(1)
      while (!numerator.isWhole) {
        numerator = numerator * 10
        denominator = denominator * 10
      }
      const[Post](numerator.toBigIntExact.get) /:/ const(denominator)
    case div @ FloatDiv(left, right) =>
      // Normally floats don't fail on division by zero, they get the `inf` value. Rewriting this to not fail on division by zero.
      implicit val o: Origin = div.o
      val newRight = dispatch(right)
      Select(newRight !== const[Post](0) /:/ const(1), RatDiv(dispatch(left), newRight)(div.blame)(div.o), getNonDetFloat())(div.o)
    case e => rewriteDefault(e)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TFloat(_, _) => TRational()(t.o)
    case t => rewriteDefault(t)
  }
}