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
  override def desc: String =
    "Converts floating point values and types into rationals, disregarding precision, nan, and other practical concerns"

  private def CastFuncOrigin(preferredName: String): Origin =
    Origin(Seq(PreferredName(Seq(preferredName)), LabelContext("float to rat")))
}

case class FloatToRat[Pre <: Generation]() extends Rewriter[Pre] {
  def name(t: TFloat[_]) =
    t match {
      case t if t == PVL.float64 => "f64"
      case t if t == PVL.float32 => "f32"
      case TFloat(e, m) => s"f${e}_$m"
    }

  private val floatOrigin: Origin = Origin(
    Seq(LabelContext("float to rational conversion"))
  )

  val specialFloats: mutable.Map[String, Function[Post]] = mutable.Map()
  val floatDiv: mutable.Map[Unit, Function[Post]] = mutable.Map()

  def NaN(implicit o: Origin): Expr[Post] = makeSpecialFloatInv("NaN")
  def posInf(implicit o: Origin): Expr[Post] = makeSpecialFloatInv("PosInf")
  def negInf(implicit o: Origin): Expr[Post] = UMinus(posInf)

  def makeSpecialFloatInv(
      name: String
  )(implicit o: Origin): FunctionInvocation[Post] = {
    val nondetFunc = specialFloats.getOrElseUpdate(name, makeSpecialFloat(name))
    FunctionInvocation[Post](nondetFunc.ref, Seq(), Nil, Nil, Nil)(
      TrueSatisfiable
    )(o)
  }

  def getFloatDiv(a: Expr[Post], b: Expr[Post])(
      implicit o: Origin
  ): FunctionInvocation[Post] = {
    val nondetFunc = floatDiv.getOrElseUpdate((), makeFloatDiv())
    FunctionInvocation[Post](nondetFunc.ref, Seq(a, b), Nil, Nil, Nil)(
      TrueSatisfiable
    )(o)
  }

  def isInf(a: Expr[Post]): Expr[Post] = {
    implicit val o: Origin = floatOrigin
    a === posInf || a === negInf
  }

  def z: Expr[Post] = {
    implicit val o: Origin = floatOrigin
    const[Post](0) /:/ const(1)
  }

  def makeFloatOp(
      body: (Expr[Post], Expr[Post]) => Expr[Post],
      name: String,
  ): Function[Post] = {
    implicit val o: Origin = floatOrigin
    val new_t = TRational[Post]()
    val a_var = new Variable[Post](new_t)(floatOrigin.where(name = "a"))
    val b_var = new Variable[Post](new_t)(floatOrigin.where(name = "b"))

    val a = Local[Post](a_var.ref)
    val b = Local[Post](b_var.ref)

    globalDeclarations.declare(
      function[Post](
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = TRational(),
        args = Seq(a_var, b_var),
        body = Some(body(a, b)),
      )(floatOrigin.where(name = name))
    )
  }

  // Normally floats don't fail on division by zero, they get the `inf` value.
  // Thus we use this function for division
  def makeFloatDiv(): Function[Post] = {
    val body =
      (a: Expr[Post], b: Expr[Post]) => {
        implicit val o: Origin = floatOrigin
        // If we want to support NaN, this would look like the following:
        // Select((b === z && a === z) || (isInf(b) && isInf(a)) || a === NaN || b === NaN, NaN,
        // Anyway I think this complicates stuff atm, since you can never really check calculations anymore, since
        // something could be NaN and then NaN keeps propagating. To properly do this we need an ADT for floats
        // Strictly speaking we need 1/inf == 0, but if we add that here, you'd always have to say that a certain value
        // is not inf. Even 5/1 does not work, since the prover thinks that 5 could be inf.

        // +/-a/0=+/-inf (also if a is inf, see:
        Select(b === z, Select(a > z, posInf, negInf), a /:/ b)
      }

    makeFloatOp(body, "floatDiv")
  }

  def makeSpecialFloat(name: String): Function[Post] = {
    globalDeclarations.declare(
      function[Post](
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = TRational(),
      )(floatOrigin.where(name = name))
    )
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case CastFloat(e, t) if e.t == t => dispatch(e)
      case CastFloat(e, t: TFloat[Pre]) if e.t.isInstanceOf[TFloat[Pre]] =>
        dispatch(e)
      case c @ CastFloat(e, t: TFloat[Pre])
          if CoercionUtils.getCoercion(e.t, TInt()).isDefined =>
        implicit val o: Origin = c.o
        dispatch(e) /:/ const(1)
      case c @ CastFloat(e, t: TInt[Pre]) if e.t.isInstanceOf[TFloat[Pre]] =>
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
      /* We should define for all operators working on floats (+, -, *, /, <, >, <=, >= !=, ==) how they interact with
       * inf and NaN. But we do not atm. This should be based on the IEEE754 standard
       * However, C/C++ programs only work with this, when the hardware actually adheres to this standard.
       * This can be checked with `#ifdef NAN`
       * So it depends on the hardware we run on. This is something we should then implement as well
       *
       * This was the best reference I could find so far:
       * https://www.gnu.org/software/libc/manual/html_node/Infinity-and-NaN.html
       *
       * To really properly address this I think float should be encoded in a ADT, and then we have separate values for
       * quiet NaN, signaling NaN , +Inf, -Inf, -Zero and Value, were value contains a rational value
       */
      case div @ FloatDiv(left, right) =>
        implicit val o: Origin = div.o
        getFloatDiv(dispatch(left), dispatch(right))
      case FloatNaN(_) => NaN(expr.o)
      case FloatInf(_) => posInf(expr.o)
      case e => e.rewriteDefault()
    }

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case TFloat(_, _) => TRational()(t.o)
      case t => t.rewriteDefault()
    }
}
