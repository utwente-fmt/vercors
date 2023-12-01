package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{CodeStringQuantifierMethod, _}
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.serialize
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults._

import scala.Int.{MaxValue, MinValue}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class RewriteContractExpr[Pre <: Generation](outer: Rewriter[Pre], givenStatementBuffer: mutable.Buffer[Statement[Rewritten[Pre]]], cls: Class[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes


  def dispatchDefault(expr: Expr[Pre]): Expr[Post] = {
    givenStatementBuffer.addOne(CodeStringStatement[Post](assertCondition(expr.toString))(expr.o))
    super.dispatch(expr)
  }


  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case _: Star[Pre] => super.dispatch(e) //TODO all the cases that have multiple expressions that needs to be taken apart in a separate expression needs to be places here
      case _: Starall[Pre] | _ : Exists[Pre] | _ : Forall[Pre]  => {
        val quantifierCall: Expr[Post] = RewriteQuantifier[Pre](this, cls).dispatch(e)
        quantifierCall match {
          case call:  CodeStringQuantifierCall[Post] => ??? //TODO
          case _ => ??? //Impossible
        }
      }
      case _ => dispatchDefault(e)
    }
  }
}
