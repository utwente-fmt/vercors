package vct.rewrite

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  Assuming,
  Expr,
  Function,
  Let,
  TBool,
  UnitAccountedPredicate,
  Variable,
}
import vct.col.origin.{DiagnosticOrigin, UnsafeDontCare}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

import vct.col.util.AstBuildHelpers._

case object EncodeAssuming extends RewriterBuilder {
  override def key: String = "encodeAssumeExpr"
  override def desc: String = "Encodes assume exprs using plain pure functions"
}

case class EncodeAssuming[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {

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
