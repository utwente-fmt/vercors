package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.lang.PVL
import vct.col.rewrite.FloatToRat.CastFuncOrigin
import vct.col.rewrite.error.ExtraNode
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

import scala.collection.mutable

case object FloatToRat extends RewriterBuilder {
  override def key: String = "floatToRat"
  override def desc: String =
    "Converts floating point values and types into rationals, disregarding precision, nan, and other practical concerns"

  case class CastFuncOrigin(preferredName: String) extends Origin {
    override val context: String = "function generated by FloatToRat"
    override val inlineContext: String = "function generated by FloatToRat"
    override val shortPosition: String = "function generated by FloatToRat"
  }
}

case class FloatToRat[Pre <: Generation]() extends Rewriter[Pre] {
  def name(t: Type[_]) =
    t match {
      case t if t == PVL.float64 => "f64"
      case t if t == PVL.float32 => "f32"
      case TFloat(e, m) => s"f${e}_$m"
    }

  def makeCast(from: Type[Pre], to: Type[Pre]): Function[Post] = {
    globalDeclarations.declare(
      function[Post](
        args = Seq(new Variable(dispatch(from))(DiagnosticOrigin)),
        returnType = dispatch(to),
        blame = PanicBlame(
          "Postcondition cannot fail for auto-generated cast function"
        ),
        contractBlame = PanicBlame(
          "Pre-condition cannot be unsatisfiable for auto-generated cast function"
        ),
      )(CastFuncOrigin(s"${name(from)}_${name(to)}"))
    )
  }

  val casts: mutable.Map[(Type[Pre], Type[Pre]), Function[Post]] = mutable.Map()

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case CastFloat(e, t) =>
        if (e.t == t) { dispatch(e) }
        else {
          val f: Function[Post] = casts
            .getOrElseUpdate((e.t, t), makeCast(e.t, t))
          implicit val o: Origin = expr.o
          FunctionInvocation(
            f.ref[Function[Post]],
            Seq(dispatch(e)),
            Nil,
            Nil,
            Nil,
          )(PanicBlame("Can always call cast on float"))
        }

      case f @ FloatValue(num, _) =>
        implicit val o = f.o
        var numerator = num
        var denominator = BigInt(1)
        while (!numerator.isWhole) {
          numerator = numerator * 10
          denominator = denominator * 10
        }
        const[Post](numerator.toBigIntExact.get) /:/ const(denominator)
      case e => rewriteDefault(e)
    }

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case TFloat(_, _) => TRational()(t.o)
      case t => rewriteDefault(t)
    }
}
