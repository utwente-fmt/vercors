package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
import vct.col.newrewrite.GivenYieldsToArgs.{MissingGivenArg, MissingYieldArg}
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.UserError

case object GivenYieldsToArgs extends RewriterBuilder {
  case class MissingGivenArg(inv: InvokingNode[_], missing: Variable[_]) extends UserError {
    override def code: String = "missingGiven"
    override def text: String =
      inv.o.messageInContext(s"This invocation is missing the 'given' parameter $missing")
  }

  case class MissingYieldArg(inv: InvokingNode[_], missing: Variable[_]) extends UserError {
    override def code: String = "missingYield"
    override def text: String =
      inv.o.messageInContext(s"This invocation is missing the 'yield' out parameter $missing")
  }
}

case class GivenYieldsToArgs[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] =>
      method.rewrite(
        args = collectInScope(variableScopes) {
          method.args.foreach(dispatch)
          method.contract.givenArgs.foreach(dispatch)
        },
        outArgs = collectInScope(variableScopes) {
          method.outArgs.foreach(dispatch)
          method.contract.yieldsArgs.foreach(dispatch)
        },
      ).succeedDefault(method)
    case func: AbstractFunction[Pre] =>
      func.rewrite(
        args = collectInScope(variableScopes) {
          func.args.foreach(dispatch)
          func.contract.givenArgs.foreach(dispatch)
        }
      ).succeedDefault(func)
    case other => rewriteDefault(other)
  }

  override def dispatch(node: ApplicableContract[Pre]): ApplicableContract[Post] =
    node.rewrite(givenArgs = Nil, yieldsArgs = Nil)

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case inv: AnyMethodInvocation[Pre] =>
      val givenMap = inv.givenMap.map { case (givenArg, value) => (givenArg.decl, value) }.toMap
      val yields = inv.yields.map { case (target, yieldArg) => (yieldArg.decl, target) }.toMap

      val orderedGivenValues = inv.ref.decl.contract.givenArgs.map(givenArg => givenMap.get(givenArg) match {
        case Some(value) => dispatch(value)
        case None => throw MissingGivenArg(inv, givenArg)
      })

      val orderedYieldTargets: Seq[Ref[Post, Variable[Post]]] = inv.ref.decl.contract.yieldsArgs.map(yieldArg => yields.get(yieldArg) match {
        case Some(value) => succ(value)
        case None => throw MissingYieldArg(inv, yieldArg)
      })

      inv.rewrite(
        args = inv.args.map(dispatch) ++ orderedGivenValues,
        outArgs = inv.outArgs.map(succ[Variable[Post]]) ++ orderedYieldTargets,
        givenMap = Nil, yields = Nil,
      )
    case inv: AnyFunctionInvocation[Pre] =>
      val givenMap = inv.givenMap.map { case (givenArg, value) => (givenArg.decl, value) }.toMap
      val orderedGivenValues = inv.ref.decl.contract.givenArgs.map(givenArg => givenMap.get(givenArg) match {
        case Some(value) => dispatch(value)
        case None => throw MissingGivenArg(inv, givenArg)
      })
      inv.rewrite(args = inv.args.map(dispatch) ++ orderedGivenValues, givenMap = Nil, yields = Nil)
    case e => rewriteDefault(e)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case inv: InvocationStatement[Pre] =>
      val givenMap = inv.givenMap.map { case (givenArg, value) => (givenArg.decl, value) }.toMap
      val yields = inv.yields.map { case (target, yieldArg) => (yieldArg.decl, target) }.toMap

      val orderedGivenValues = inv.ref.decl.contract.givenArgs.map(givenArg => givenMap.get(givenArg) match {
        case Some(value) => dispatch(value)
        case None => throw MissingGivenArg(inv, givenArg)
      })

      val orderedYieldTargets: Seq[Ref[Post, Variable[Post]]] = inv.ref.decl.contract.yieldsArgs.map(yieldArg => yields.get(yieldArg) match {
        case Some(value) => succ(value)
        case None => throw MissingYieldArg(inv, yieldArg)
      })

      inv.rewrite(
        args = inv.args.map(dispatch) ++ orderedGivenValues,
        outArgs = inv.outArgs.map(succ[Variable[Post]]) ++ orderedYieldTargets,
        givenMap = Nil, yields = Nil,
      )
    case other => rewriteDefault(other)
  }
}
