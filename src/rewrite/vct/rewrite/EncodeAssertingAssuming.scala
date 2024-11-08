package vct.rewrite

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  Asserting,
  Assuming,
  Expr,
  Function,
  Let,
  TBool,
  Type,
  UnitAccountedPredicate,
  Variable,
}
import vct.col.origin.{
  AssertFailed,
  Blame,
  DiagnosticOrigin,
  InvocationFailure,
  Origin,
  PreconditionFailed,
  UnsafeDontCare,
}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

import scala.collection.mutable
import vct.col.util.AstBuildHelpers._
import vct.rewrite.EncodeAssertingAssuming.InvocationFailureToAssertFailed

case object EncodeAssertingAssuming extends RewriterBuilder {
  override def key: String = "encodeAssertAssumeExpr"
  override def desc: String =
    "Encodes assert/assume exprs using plain pure functions"

  case class InvocationFailureToAssertFailed(assertExpr: Asserting[_])
      extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(path, failure, node) =>
          assertExpr.blame.blame(AssertFailed(failure, assertExpr.assn))
        case _ => ???
      }
  }
}

case class EncodeAssertingAssuming[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {

  lazy val assertingFunction: Function[Post] = {
    implicit val o = DiagnosticOrigin
    val assnVar = new Variable[Post](TBool())(o.where(name = "assn"))
    globalDeclarations.declare(
      function(
        blame = UnsafeDontCare.Contract("assumption primitive"),
        contractBlame = UnsafeDontCare.Satisfiability("assumption primitive"),
        returnType = TBool(),
        args = Seq(assnVar),
        requires = UnitAccountedPredicate(assnVar.get),
      )(o.where(name = "assert_"))
    )
  }

  lazy val assumingFunction: Function[Post] = {
    implicit val o = DiagnosticOrigin
    val assnVar = new Variable[Post](TBool())(o.where(name = "assn"))
    globalDeclarations.declare(
      function(
        blame = UnsafeDontCare.Contract("assumption primitive"),
        contractBlame = UnsafeDontCare.Satisfiability("assumption primitive"),
        returnType = TBool(),
        args = Seq(assnVar),
        ensures = UnitAccountedPredicate(assnVar.get),
      )(o.where(name = "assume_"))
    )
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case expr @ Asserting(assn, inner) =>
        implicit val o = expr.o
        Let(
          new Variable(TBool())(o.where(name = "_")),
          functionInvocation(
            ref = assertingFunction.ref,
            args = Seq(dispatch(assn)),
            blame = InvocationFailureToAssertFailed(expr),
          ),
          dispatch(inner),
        )

      case Assuming(assn, inner) =>
        implicit val o = expr.o
        Let(
          new Variable(TBool())(o.where(name = "_")),
          functionInvocation(
            ref = assumingFunction.ref,
            args = Seq(dispatch(assn)),
            blame = UnsafeDontCare.Invocation("assumption"),
          ),
          dispatch(inner),
        )

      case _ => expr.rewriteDefault()
    }
}
