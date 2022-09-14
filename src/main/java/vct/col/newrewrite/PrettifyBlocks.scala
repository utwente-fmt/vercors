package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import RewriteHelpers._
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable.ArrayBuffer

case class PrettifyBlocks[Pre <: Generation]() extends Rewriter[Pre] {
  def collectVariables(body: Statement[Pre], extra: Seq[Variable[Pre]] = Nil): Statement[Post] = {
    val (variablesHere, newBody) = variables.collect {
      extra.foreach(dispatch)
      dispatch(body)
    }

    Scope(variablesHere, newBody)(body.o)
  }

  def dispatchFlatly(stat: Statement[Pre]): Seq[Statement[Post]] = stat match {
    case Block(statements) => statements.flatMap(dispatchFlatly)
    case Scope(Nil, body) => dispatchFlatly(body)
    case Scope(locals, body) if variables.nonEmpty =>
      locals.foreach(dispatch)
      dispatchFlatly(body)
    case other => Seq(dispatch(other))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Block(statements) =>
      Block(statements.flatMap(dispatchFlatly))(stat.o)
    case Scope(Nil, body) => dispatch(body)
    case Scope(locals, body) =>
      if(variables.nonEmpty) {
        locals.foreach(dispatch)
        dispatch(body)
      } else {
        collectVariables(body, extra = locals)
      }

    case vec: VecBlock[Pre] =>
      vec.rewrite(content = collectVariables(vec.content))

    case act: ModelDo[Pre] =>
      act.rewrite(impl = collectVariables(act.impl))

    case pack: WandPackage[Pre] =>
      pack.rewrite(proof = collectVariables(pack.proof))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case ScopedExpr(locals, body) =>
      locals.foreach(dispatch)
      dispatch(body)
    case other => rewriteDefault(other)
  }

  override def dispatch(parRegion: ParRegion[Pre]): ParRegion[Rewritten[Pre]] = parRegion match {
    case block: ParBlock[Pre] =>
      block.rewrite(content = collectVariables(block.content))
    case other => rewriteDefault(other)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: RunMethod[Pre] =>
      classDeclarations.succeed(method, method.rewrite(body = method.body.map(collectVariables(_))))
    case method: AbstractMethod[Pre] =>
      allScopes.anySucceedOnly(method,
        allScopes.anyDeclare(
          method.rewrite(body = method.body.map(collectVariables(_)))))
    case other => rewriteDefault(other)
  }
}
