package vct.rewrite

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  AssertExpr,
  AssumeExpr,
  Expr,
  Function,
  FunctionInvocation,
  TBool,
  Type,
  UnitAccountedPredicate,
  Variable,
}
import vct.col.origin.{
  AssertFailed,
  Blame,
  InvocationFailure,
  PreconditionFailed,
  UnsafeDontCare,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

import scala.collection.mutable
import vct.col.util.AstBuildHelpers._
import vct.rewrite.EncodeAssertAssumeExpr.InvocationFailureToAssertFailed

case object EncodeAssertAssumeExpr extends RewriterBuilder {
  override def key: String = "encodeAssertAssumeExpr"
  override def desc: String =
    "Encodes assert/assume exprs using plain pure functions"

  case class InvocationFailureToAssertFailed(assertExpr: AssertExpr[_])
      extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(path, failure, node) =>
          assertExpr.blame.blame(AssertFailed(failure, assertExpr.assn))
        case _ => ???
      }
  }
}

case class EncodeAssertAssumeExpr[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {

  val assertFunctions: mutable.Map[Type[Pre], Function[Post]] = mutable.Map()
  val assumeFunctions: mutable.Map[Type[Pre], Function[Post]] = mutable.Map()

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case expr @ AssertExpr(assn, inner) =>
        implicit val o = expr.o
        val f = assertFunctions.getOrElse(
          inner.t, {
            val assnVar =
              new Variable[Post](TBool())(expr.o.where(name = "assn"))
            val innerVar =
              new Variable[Post](dispatch(inner.t))(
                expr.o.where(name = "inner")
              )
            withResult[Post, Function[Post]] { res =>
              globalDeclarations.declare(
                function(
                  blame = UnsafeDontCare.Contract("assumption primitive"),
                  contractBlame = UnsafeDontCare
                    .Satisfiability("assumption primitive"),
                  returnType = dispatch(inner.t),
                  args = Seq(assnVar, innerVar),
                  requires = UnitAccountedPredicate(assnVar.get),
                  ensures = UnitAccountedPredicate(res === innerVar.get),
                )(expr.o.where(name = "asserting"))
              )
            }
          },
        )
        functionInvocation(
          ref = f.ref,
          args = Seq(dispatch(assn), dispatch(inner)),
          blame = InvocationFailureToAssertFailed(expr),
        )

      case AssumeExpr(
            assn,
            inner,
          ) => // (\assume assn; inner) === ensures assn && inner == \result; f(assn, inner)
        implicit val o = expr.o
        val f = assumeFunctions.getOrElse(
          inner.t, {
            val assnVar =
              new Variable[Post](TBool())(expr.o.where(name = "assn"))
            val innerVar =
              new Variable[Post](dispatch(inner.t))(
                expr.o.where(name = "inner")
              )
            withResult[Post, Function[Post]] { res =>
              globalDeclarations.declare(
                function(
                  blame = UnsafeDontCare.Contract("assumption primitive"),
                  contractBlame = UnsafeDontCare
                    .Satisfiability("assumption primitive"),
                  returnType = dispatch(inner.t),
                  args = Seq(assnVar, innerVar),
                  ensures = UnitAccountedPredicate(
                    assnVar.get && (res === innerVar.get)
                  ),
                )(expr.o.where(name = "assuming"))
              )
            }
          },
        )
        functionInvocation(
          ref = f.ref,
          args = Seq(dispatch(assn), dispatch(inner)),
          blame = UnsafeDontCare.Invocation("assumption"),
        )

      case _ => expr.rewriteDefault()
    }
}
