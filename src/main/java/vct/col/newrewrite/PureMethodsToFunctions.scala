package vct.col.newrewrite

import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.ast.RewriteHelpers._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.UserError

case object PureMethodsToFunctions extends RewriterBuilder {
  override def key: String = "pureMethods"
  override def desc: String = "Compile methods marked as pure into functions."

  case object PureMethodOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def shortPosition: String = "generated"
    override def context: String = "[At node generated for pure method]"
    override def inlineContext: String = "[Node generated for pure method]"
  }

  case class MethodCannotIntoFunction(explanation: String) extends UserError {
    override def code: String = "notPure"
    override def text: String =
      s"This method is marked as pure, but it cannot be converted to a function, since $explanation."
  }
}

case class PureMethodsToFunctions[Pre <: Generation]() extends Rewriter[Pre] {
  import PureMethodsToFunctions._

  def toExpression(stat: Statement[Pre], alt: => Option[Expr[Post]]): Option[Expr[Post]] = {
    implicit val o: Origin = DiagnosticOrigin
    stat match {
      case Return(e) => Some(dispatch(e))
      case Block(Nil) => alt
      case Block(stat :: tail) =>
        toExpression(stat, toExpression(Block(tail), alt))
      case Branch(Nil) => alt
      case Branch((BooleanValue(true), impl) +: _) => toExpression(impl, alt)
      case Branch((cond, impl) +: branches) =>
        Some(Select(dispatch(cond),
          toExpression(impl, alt).getOrElse(return None),
          toExpression(Branch(branches), alt).getOrElse(return None)))
      case Scope(_, impl) => toExpression(impl, alt)
      case _ => None
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case proc: Procedure[Pre] if proc.pure =>
        if(proc.outArgs.nonEmpty) throw MethodCannotIntoFunction("the method has out parameters")
        if(proc.contract.signals.nonEmpty) throw MethodCannotIntoFunction("the method contract contains a signals declaration")
        new Function(
          returnType = dispatch(proc.returnType),
          args = collectInScope(variableScopes) { proc.args.foreach(dispatch) },
          typeArgs = collectInScope(variableScopes) { proc.typeArgs.foreach(dispatch) },
          body = proc.body.map(toExpression(_, None).getOrElse(throw MethodCannotIntoFunction(
            "the method implementation cannot be restructured into a pure expression"))),
          contract = dispatch(proc.contract),
          inline = proc.inline
        )(proc.blame)(proc.o).succeedDefault(proc)
      case method: InstanceMethod[Pre] if method.pure =>
        if(method.outArgs.nonEmpty) throw MethodCannotIntoFunction("the method has out parameters")
        if(method.contract.signals.nonEmpty) throw MethodCannotIntoFunction("the method contract contains a signals declaration")
        new InstanceFunction(
          returnType = dispatch(method.returnType),
          args = collectInScope(variableScopes) { method.args.foreach(dispatch) },
          typeArgs = collectInScope(variableScopes) { method.typeArgs.foreach(dispatch) },
          body = method.body.map(toExpression(_, None).getOrElse(throw MethodCannotIntoFunction(
            "the method implementation cannot be restructured into a pure expression"))),
          contract = dispatch(method.contract),
          inline = method.inline,
        )(method.blame)(method.o).succeedDefault(method)
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case inv @ ProcedureInvocation(Ref(proc), args, outArgs, typeArgs, givenMap, yields) =>
      if(proc.pure)
        FunctionInvocation[Post](succ(proc), args.map(dispatch), typeArgs.map(dispatch),
          givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)(e.o)
      else rewriteDefault(inv)
    case inv @ MethodInvocation(obj, Ref(method), args, outArgs, typeArgs, givenMap, yields) =>
      if(method.pure)
        InstanceFunctionInvocation[Post](dispatch(obj), succ(method), args.map(dispatch), typeArgs.map(dispatch),
          givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
          yields.map { case (Ref(e), Ref(v)) => (succ(e), succ(v)) })(inv.blame)(e.o)
      else rewriteDefault(inv)
    case other => rewriteDefault(other)
  }
}