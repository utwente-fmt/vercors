package vct.col.newrewrite

import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.Rewriter
import vct.col.ast.RewriteHelpers._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.UserError

case object PureMethodsToFunctions {
  case object PureMethodOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def messageInContext(message: String): String =
      s"[At node generated for pure method]: $message"
  }

  case class MethodCannotIntoFunction(explanation: String) extends UserError {
    override def code: String = "notPure"
    override def text: String =
      s"This method is marked as pure, but it cannot be converted to a function, since $explanation."
  }
}

case class PureMethodsToFunctions() extends Rewriter {
  import PureMethodsToFunctions._

  def toExpression(stat: Statement, alt: => Option[Expr]): Option[Expr] = {
    implicit val o: Origin = DiagnosticOrigin
    stat match {
      case Return(e) => Some(dispatch(e))
      case Block(Nil) => alt
      case Block(stat :: tail) =>
        toExpression(stat, toExpression(Block(tail), alt))
      case Branch(Nil) => alt
      case Branch((`tt`, impl) +: _) => toExpression(impl, alt)
      case Branch((cond, impl) +: branches) =>
        Some(Select(dispatch(cond),
          toExpression(impl, alt).getOrElse(return None),
          toExpression(Branch(branches), alt).getOrElse(return None)))
      case Scope(_, impl) => toExpression(impl, alt)
      case _ => None
    }
  }

  override def dispatch(decl: Declaration): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case proc: Procedure if proc.pure =>
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
        )(proc.blame)(proc.o).succeedDefault(this, proc)
      case method: InstanceMethod if method.pure =>
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
        )(method.blame)(method.o).succeedDefault(this, method)
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(e: Expr): Expr = e match {
    case inv @ ProcedureInvocation(Ref(proc), args, outArgs, typeArgs) =>
      if(proc.pure)
        FunctionInvocation(succ(proc), args.map(dispatch), typeArgs.map(dispatch))(inv.blame)(e.o)
      else rewriteDefault(inv)
    case inv @ MethodInvocation(obj, Ref(method), args, outArgs, typeArgs) =>
      if(method.pure)
        InstanceFunctionInvocation(dispatch(obj), succ(method), args.map(dispatch), typeArgs.map(dispatch))(inv.blame)(e.o)
      else rewriteDefault(inv)
    case other => rewriteDefault(other)
  }
}