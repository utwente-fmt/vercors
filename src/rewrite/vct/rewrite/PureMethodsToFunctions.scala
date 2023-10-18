package vct.col.rewrite

import hre.util.ScopedStack
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

  def countAssignments(v: Variable[Pre], s: Statement[Pre]): Option[Int] =
    s match {
      case Return(_) => Some(0)
      case Block(stats) =>
        val x =
          stats.map(countAssignments(v, _)).fold(Some(0)) {
            case (Some(a), Some(b)) => Some(a + b)
            case _ => None
          }
        x
      case Branch(conds) =>
        val assignmentCounts = conds.map(_._2).map(countAssignments(v, _))
          .collect {
            case Some(n) => n
            case None => return None
          }
        if (assignmentCounts.forall(_ <= 1)) {
          assignmentCounts.maxOption.orElse(Some(0))
        } else { None }
      case Assign(Local(ref), _) =>
        Some(
          if (ref.decl == v)
            1
          else
            0
        )
      case Assign(_, _) => None
      case _ => None
    }

  def toExpression(
      stat: Statement[Pre],
      alt: => Option[Expr[Post]],
  ): Option[Expr[Post]] = {
    implicit val o: Origin = DiagnosticOrigin
    stat match {
      case Return(e) =>
        alt match {
          case Some(_) =>
            throw MethodCannotIntoFunction(
              currentAbstractMethod.top,
              "Dead code after return is not allowed in pure methods",
            )
          case None => Some(dispatch(e))
        }
      case Block(Nil) => alt
      case Block(stat +: tail) =>
        toExpression(stat, toExpression(Block(tail), alt))
      case Branch(Nil) => alt
      case Branch((BooleanValue(true), impl) +: _) => toExpression(impl, alt)
      case Branch((cond, impl) +: branches) =>
        Some(Select(
          dispatch(cond),
          toExpression(impl, alt).getOrElse(return None),
          toExpression(Branch(branches), alt).getOrElse(return None),
        ))
      case Scope(locals, impl) =>
        if (!locals.forall(countAssignments(_, impl).exists(_ <= 1))) {
          return None
        }
        toExpression(impl, alt)
      case Assign(Local(ref), e) =>
        alt match {
          case Some(exprAlt) =>
            Some(Let[Post](
              variables.collect { dispatch(ref.decl) }._1.head,
              dispatch(e),
              exprAlt,
            ))
          case None =>
            throw MethodCannotIntoFunction(
              currentAbstractMethod.top,
              "Pure method cannot end with assign statement",
            )
        }

      case _ => None
    }
  }

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
                  toExpression(_, None).getOrElse(
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
                  toExpression(_, None).getOrElse(
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
