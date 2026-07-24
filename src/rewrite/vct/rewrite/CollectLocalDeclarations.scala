package vct.rewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.Unreachable

case object CollectLocalDeclarations extends RewriterBuilder {
  override def key: String = "collectLocalDecls"
  override def desc: String = "Collect declaration statements into a scope."
}

case class CollectLocalDeclarations[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case LocalDecl(local) =>
        if (variables.isEmpty) {
          throw Unreachable(
            "The frontend must not open an execution context (e.g. method body) that does not immediately contain a scope."
          )
        }

        variables.succeed(local, local.rewrite())
        Block(Nil)(stat.o)
      case Scope(vars, impl) =>
        val (newVars, newImpl) = variables.collect {
          vars.foreach(dispatch)
          dispatch(impl)
        }
        Scope[Post](newVars, newImpl)(stat.o)
      case _ => super.dispatch(stat)
    }
}
