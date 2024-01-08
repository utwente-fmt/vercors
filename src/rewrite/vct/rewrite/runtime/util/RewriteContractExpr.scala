package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable


case class RewriteContractExpr[Pre <: Generation](outer: Rewriter[Pre], cls: Class[Pre])(implicit program: Program[Pre], newLocals: NewVariableResult[Pre, _]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  private val internalExpression: ScopedStack[Expr[Pre]] = new ScopedStack()
  private val givenStatementBuffer: mutable.Buffer[Statement[Rewritten[Pre]]] = new mutable.ArrayBuffer[Statement[Rewritten[Pre]]]()

  def createStatements(expr: Expr[Pre]): (Expr[Post], mutable.Buffer[Statement[Rewritten[Pre]]]) = {
    val unfoldedPredicate: Seq[Expr[Pre]] = unfoldStar(expr)
    println(unfoldedPredicate)
    val newExpr = dispatch(expr)
    (newExpr, givenStatementBuffer)
  }

  def createStatement(expr: Expr[Post]): Expr[Post] = {
    givenStatementBuffer.addOne(Assert[Post](expr)(null)(Origin(Seq())))
    expr
  }

  def dispatchPermission(p: Perm[Pre]) : Expr[Post] = {
    PermissionRewriter(this).rewritePermission(p)
  }


  def canSplit(e: Expr[Pre]): Boolean = {
    e match {
      case _: Star[Pre] => true
      case _: Starall[Pre] | _ : Exists[Pre] | _ : Forall[Pre] => false
      case _: Perm[Pre] => false
      case _: Or[Pre] => false
      case _: AmbiguousOr[Pre] => false
      case _: Implies[Pre] => false
      case _: OrderOp[Pre] => false
      case _: BinExpr[Pre] => true
      case _ => true
    }
  }


  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    val result = e match {
      case _: Star[Pre] => super.dispatch(e) //TODO all the cases that have multiple expressions that needs to be taken apart in a separate expression needs to be places here
      case _: Or[Pre] => internalExpression.having(e){super.dispatch(e)}
      case _: AmbiguousOr[Pre] => internalExpression.having(e){super.dispatch(e)}
      case _: Starall[Pre] | _ : Exists[Pre] | _ : Forall[Pre]  => RewriteQuantifier[Pre](this, cls).dispatch(e)
      case p: Perm[Pre] => dispatchPermission(p)
      case _ => super.dispatch(e)
    }

    if(internalExpression.isEmpty && !canSplit(e)){
      createStatement(result)
    }
    result
  }
}
