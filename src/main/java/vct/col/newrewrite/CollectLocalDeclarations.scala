package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.result.VerificationResult.Unreachable

import scala.collection.mutable.ArrayBuffer

case object CollectLocalDeclarations extends RewriterBuilder

case class CollectLocalDeclarations[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case LocalDecl(local) =>
      if(variableScopes.isEmpty) {
        throw Unreachable("The frontend must not open an execution context (e.g. method body) that does not immediately contain a scope.")
      }

      local.rewrite().succeedDefault(this, local)
      Block(Nil)(stat.o)
    case Scope(vars, impl) =>
      val newVars = ArrayBuffer[Variable[Post]]()
      val newImpl = variableScopes.having(newVars) {
        vars.foreach(dispatch)
        dispatch(impl)
      }
      Scope[Post](newVars.toIndexedSeq, newImpl)(stat.o)
    case other => rewriteDefault(other)
  }
}