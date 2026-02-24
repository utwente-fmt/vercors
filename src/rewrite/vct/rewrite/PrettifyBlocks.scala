package vct.rewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.Unreachable

object PrettifyBlocks extends RewriterBuilder {
  override def key: String = "prettifyBlocks"
  override def desc: String =
    "Flattens nested blocks/scopes and removes empty blocks/scopes"
}

case class PrettifyBlocks[Pre <: Generation]() extends Rewriter[Pre] {
  private def dispatchFlatly(stat: Statement[Pre]): Seq[Statement[Post]] =
    stat match {
      case Block(statements) => statements.flatMap(dispatchFlatly)
      case Scope(Nil, Block(Nil)) => Nil
      case other => Seq(dispatch(other))
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case Block(statements) =>
        Block(statements.flatMap(dispatchFlatly))(stat.o)
      case Scope(Nil, Block(Nil)) => Block[Post](Nil)(stat.o)
      case Scope(locals, body) =>
        val (variablesHere, newBody) = variables.collect {
          locals.foreach(dispatch)
          dispatch(body)
        }
        Scope(variablesHere, newBody)(body.o)
      case _ => super.dispatch(stat)
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case ScopedExpr(locals, body) if variables.nonEmpty =>
        locals.foreach(dispatch)
        dispatch(body)
      case ScopedExpr(_, _) =>
        throw Unreachable(
          s"Got to this scoped expression (`$e`) but there is no outer scope. Every function/method body should start with a Scope node"
        )
      case _ => super.dispatch(e)
    }
}
