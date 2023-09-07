package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.result.VerificationError.Unreachable

import scala.collection.mutable.ArrayBuffer

case object CollectLocalDeclarations extends RewriterBuilder {
  override def key: String = "collectLocalDecls"
  override def desc: String = "Collect declaration statements into a scope."
}

case class CollectLocalDeclarations[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case LocalDecl(local) =>
      if(variables.isEmpty) {
        throw Unreachable("The frontend must not open an execution context (e.g. method body) that does not immediately contain a scope.")
      }

      variables.succeed(local, local.rewrite())
      Block(Nil)(stat.o)
    case Scope(vars, impl) =>
      val (newVars, newImpl) = variables.collect {
        vars.foreach(dispatch)
        dispatch(impl)
      }
      Scope[Post](newVars, newImpl)(stat.o)
    case Loop(_, _, _, _, _: Scope[Pre]) => rewriteDefault(stat)
    case loop@Loop(_, _, _, _, body) =>
      val (newVars, newBody) = variables.collect {
        dispatch(body)
      }
      val bodyScope = Scope[Post](newVars, newBody)(body.o)
      loop.rewrite(body = bodyScope)
    case other => rewriteDefault(other)
  }
}