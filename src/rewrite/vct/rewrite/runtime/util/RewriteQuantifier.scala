package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers._
import vct.col.ast.{Expr, Variable, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter}
import vct.col.util.AstBuildHelpers.{ff, tt}



case class RewriteQuantifier[Pre <: Generation](override val outer: Rewriter[Pre], override val cls: Class[Pre])(implicit program: Program[Pre], newLocals: NewVariableResult[Pre, _]) extends AbstractQuantifierRewriter[Pre](outer, cls){


  def defineLoopAssertion(expr: Expr[Pre], condition: Expr[Pre]): Branch[Post] = {
    val loopAssertion = expr match {
      case _: Forall[Pre] => (Not[Post](dispatch(condition))(expr.o), Return[Post](BooleanValue(false)(expr.o))(expr.o))
      case _: Starall[Pre] => (Not[Post](dispatch(condition))(expr.o), Return[Post](BooleanValue(false)(expr.o))(expr.o)) //TODO fix separation conjunction that it keeps track of it if it is the same variable or not
      case _: Exists[Pre] => (dispatch(condition), Return[Post](BooleanValue(true)(expr.o))(expr.o))
      case _ => ???
    }
    Branch[Post](Seq(loopAssertion))(expr.o)
  }

  override def dispatchLoopBody(quantifier: Expr[Pre], left: Expr[Pre], right: Expr[Pre]): Seq[Statement[Post]] = Seq(defineLoopAssertion(quantifier, right))

  override def dispatchPostBody(quantifier: Expr[Pre], bindings: Seq[Variable[Pre]], left: Expr[Pre], right: Expr[Pre], quantifierId: String, newLocals: Seq[Variable[Post]]): Seq[Statement[Post]] = {
    implicit val origin: Origin = quantifier.o
    val postReturn = quantifier match {
      case _: Forall[Pre] =>  Return[Post](tt)
      case _: Starall[Pre] => Return[Post](tt)
      case _: Exists[Pre] => Return[Post](ff)
      case _ => ???
    }
    Seq(postReturn)
  }
}
