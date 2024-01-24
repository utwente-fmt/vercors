package vct.rewrite.runtime.util

import vct.col.ast.{Expr, Variable, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.permissionTransfer.PermissionData


case class RewriteQuantifier[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](pd) {


  override def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = {
    val loopAssertion = defineLoopAssertion(loopBodyContent.quantifier, loopBodyContent.expr)
    val postBody = dispatchPostBody(loopBodyContent.quantifier)
    Block[Post](Seq(loopAssertion, postBody))
  }

  def defineLoopAssertion(expr: Expr[Pre], condition: Expr[Pre]): Branch[Post] = {
    implicit val origin: Origin = expr.o
    val con = dispatch(condition)

    val loopAssertion = expr match {
      case _: Forall[Pre] => (!con, Return[Post](ff))
      case _: Starall[Pre] => (!con, Return[Post](ff)) //TODO fix separation conjunction that it keeps track of it if it is the same variable or not
      case _: Exists[Pre] => (con, Return[Post](ff))
      case _ => ???
    }
    Branch[Post](Seq(loopAssertion))(expr.o)
  }

  def dispatchPostBody(quantifier: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = quantifier.o
    val postReturn = quantifier match {
      case _: Forall[Pre] => Return[Post](tt)
      case _: Starall[Pre] => Return[Post](tt)
      case _: Exists[Pre] => Return[Post](ff)
      case _ => ???
    }
    postReturn
  }
}
