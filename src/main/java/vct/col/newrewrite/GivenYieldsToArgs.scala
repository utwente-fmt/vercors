package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
import vct.col.newrewrite.GivenYieldsToArgs.{MissingGivenArg, YieldDummy}
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

case object GivenYieldsToArgs extends RewriterBuilder {
  override def key: String = "givenYields"
  override def desc: String = "Promote given and yields parameters into inlined arguments and out-parameters."

  case class MissingGivenArg(inv: InvokingNode[_], missing: Variable[_]) extends UserError {
    override def code: String = "missingGiven"
    override def text: String =
      inv.o.messageInContext(s"This invocation is missing the 'given' parameter $missing")
  }

  case class YieldDummy(forArg: Variable[_]) extends Origin {
    override def preferredName: String = "dummy_" + forArg.o.preferredName
    override def shortPosition: String = "generated"
    override def context: String = "[At dummy variable for an unused out parameter]"
    override def inlineContext: String = "[Dummy variable for an unused out parameter]"
  }
}

case class GivenYieldsToArgs[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] =>
      allScopes.anySucceedOnly(method, allScopes.anyDeclare(method.rewrite(
        args = variables.collect {
          method.args.foreach(dispatch)
          method.contract.givenArgs.foreach(dispatch)
        }._1,
        outArgs = variables.collect {
          method.outArgs.foreach(dispatch)
          method.contract.yieldsArgs.foreach(dispatch)
        }._1,
      )))
    case func: AbstractFunction[Pre] =>
      allScopes.anySucceedOnly(func, allScopes.anyDeclare(func.rewrite(
        args = variables.collect {
          func.args.foreach(dispatch)
          func.contract.givenArgs.foreach(dispatch)
        }._1,
      )))
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

      val (yieldDummies, orderedYieldTargets: Seq[Ref[Post, Variable[Post]]]) =
        variables.collect {
          inv.ref.decl.contract.yieldsArgs.map(yieldArg => yields.get(yieldArg) match {
            case Some(value) => succ[Variable[Post]](value.decl)
            case None => variables.declare(new Variable[Post](dispatch(yieldArg.t))(YieldDummy(yieldArg))).ref
          })
        }

      val newInv = inv.rewrite(
        args = inv.args.map(dispatch) ++ orderedGivenValues,
        outArgs = inv.outArgs.map(arg => succ[Variable[Post]](arg.decl)) ++ orderedYieldTargets,
        givenMap = Nil, yields = Nil,
      )

      yieldDummies match {
        case Nil => newInv
        case _ => ScopedExpr(yieldDummies, newInv)(e.o)
      }
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
        case Some(value) => succ(value.decl)
        case None => variables.declare(new Variable[Post](dispatch(yieldArg.t))(YieldDummy(yieldArg))).ref
      })

      inv.rewrite(
        args = inv.args.map(dispatch) ++ orderedGivenValues,
        outArgs = inv.outArgs.map(arg => succ[Variable[Post]](arg.decl)) ++ orderedYieldTargets,
        givenMap = Nil, yields = Nil,
      )
    case other => rewriteDefault(other)
  }
}
