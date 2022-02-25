package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import RewriteHelpers._
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case class PrettifyBlocks[Pre <: Generation]() extends Rewriter[Pre] {
  val haveTopScope: ScopedStack[Unit] = ScopedStack()

  def collectVariables(body: Statement[Pre], extra: Seq[Variable[Pre]] = Nil): Statement[Post] = {
    val variables = ArrayBuffer[Variable[Post]]()

    val newBody = variableScopes.having(variables) {
      extra.foreach(dispatch)
      dispatch(body)
    }

    Scope(variables.toIndexedSeq, newBody)(body.o)
  }

  def dispatchFlatly(stat: Statement[Pre]): Seq[Statement[Post]] = stat match {
    case Block(statements) => statements.flatMap(dispatchFlatly)
    case Scope(Nil, body) => dispatchFlatly(body)
    case Scope(locals, body) if variableScopes.nonEmpty =>
      locals.foreach(dispatch)
      dispatchFlatly(body)
    case other => Seq(dispatch(other))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Block(statements) =>
      Block(statements.flatMap(dispatchFlatly))(stat.o)
    case Scope(Nil, body) => dispatch(body)
    case Scope(locals, body) =>
      if(variableScopes.nonEmpty) {
        locals.foreach(dispatch)
        dispatch(body)
      } else {
        collectVariables(body, extra = locals)
      }

    case vec: VecBlock[Pre] =>
      vec.rewrite(content = collectVariables(vec.content))

    case act: ModelDo[Pre] =>
      act.rewrite(impl = collectVariables(act.impl))

    case other => rewriteDefault(other)
  }

  override def dispatch(parRegion: ParRegion[Pre]): ParRegion[Rewritten[Pre]] = parRegion match {
    case block: ParBlock[Pre] =>
      block.rewrite(content = collectVariables(block.content))
    case other => rewriteDefault(other)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] =>
      method.rewrite(body = method.body.map(collectVariables(_))).succeedDefault(method)
    case other => rewriteDefault(other)
  }
}
