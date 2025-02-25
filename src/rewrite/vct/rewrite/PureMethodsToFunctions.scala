package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, LabelContext, Origin}
import vct.col.ref.Ref
import vct.col.util.StatementToExpression
import vct.result.VerificationError.UserError

case object PureMethodsToFunctions extends RewriterBuilder {
  override def key: String = "pureMethods"
  override def desc: String = "Compile methods marked as pure into functions."

  val PureMethodOrigin: Origin = Origin(Seq(LabelContext("pure method")))

  case class MethodCannotIntoFunction(
      method: AbstractMethod[_],
      explanation: String,
  ) extends UserError {
    override def code: String = "notPure"
    override def text: String =
      method.o.messageInContext(
        s"This method is marked as pure, but it cannot be converted to a function, since $explanation."
      )
  }
}

case class PureMethodsToFunctions[Pre <: Generation]() extends Rewriter[Pre] {
  import PureMethodsToFunctions._

  val currentAbstractMethod: ScopedStack[AbstractMethod[Pre]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case proc: Procedure[Pre] if proc.pure =>
        if (proc.outArgs.nonEmpty)
          throw MethodCannotIntoFunction(proc, "the method has out parameters")
        if (proc.contract.signals.nonEmpty)
          throw MethodCannotIntoFunction(
            proc,
            "the method contract contains a signals declaration",
          )
        globalDeclarations.succeed(
          proc,
          new Function(
            returnType = dispatch(proc.returnType),
            args = variables.dispatch(proc.args),
            typeArgs = variables.dispatch(proc.typeArgs),
            body =
              currentAbstractMethod.having(proc) {
                proc.body.map(
                  StatementToExpression(
                    this,
                    (s: String) => MethodCannotIntoFunction(proc, s),
                  ).toExpression(_, None).getOrElse(
                    throw MethodCannotIntoFunction(
                      proc,
                      "the method implementation cannot be restructured into a pure expression",
                    )
                  )
                )
              },
            contract = dispatch(proc.contract),
            inline = proc.inline,
          )(proc.blame)(proc.o),
        )
      case method: InstanceMethod[Pre] if method.pure =>
        if (method.outArgs.nonEmpty)
          throw MethodCannotIntoFunction(
            method,
            "the method has out parameters",
          )
        if (method.contract.signals.nonEmpty)
          throw MethodCannotIntoFunction(
            method,
            "the method contract contains a signals declaration",
          )
        classDeclarations.succeed(
          method,
          new InstanceFunction(
            returnType = dispatch(method.returnType),
            args = variables.dispatch(method.args),
            typeArgs = variables.dispatch(method.typeArgs),
            body =
              currentAbstractMethod.having(method) {
                method.body.map(
                  StatementToExpression(
                    this,
                    (s: String) => MethodCannotIntoFunction(method, s),
                  ).toExpression(_, None).getOrElse(
                    throw MethodCannotIntoFunction(
                      method,
                      "the method implementation cannot be restructured into a pure expression",
                    )
                  )
                )
              },
            contract = dispatch(method.contract),
            inline = method.inline,
          )(method.blame)(method.o),
        )
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case inv @ ProcedureInvocation(
            Ref(proc),
            args,
            outArgs,
            typeArgs,
            givenMap,
            yields,
          ) =>
        if (proc.pure)
          FunctionInvocation[Post](
            succ(proc),
            args.map(dispatch),
            typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (e, Ref(v)) => (dispatch(e), succ(v)) },
          )(inv.blame)(e.o)
        else
          rewriteDefault(inv)
      case inv @ MethodInvocation(
            obj,
            Ref(method),
            args,
            outArgs,
            typeArgs,
            givenMap,
            yields,
          ) =>
        if (method.pure)
          InstanceFunctionInvocation[Post](
            dispatch(obj),
            succ(method),
            args.map(dispatch),
            typeArgs.map(dispatch),
            givenMap.map { case (Ref(v), e) => (succ(v), dispatch(e)) },
            yields.map { case (e, Ref(v)) => (dispatch(e), succ(v)) },
          )(inv.blame)(e.o)
        else
          rewriteDefault(inv)
      case other => rewriteDefault(other)
    }
}
