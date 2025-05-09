package vct.rewrite.pallas

import vct.col.ast._
import vct.col.origin.{LabelContext, Origin, PanicBlame, PreferredName}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.assignLocal
import vct.col.util.{StatementToExpression, SubstituteReferences}
import vct.result.Message
import vct.result.VerificationError.SystemError
import vct.rewrite.pallas.InlinePallasWrappers.{
  InlineArgAssignOrigin,
  WrapperInlineFailed,
}

case object InlinePallasWrappers extends RewriterBuilder {
  override def key: String = "inlinePallasWrapper"
  override def desc: String =
    "Inline calls to wrapper-function in pallas specifications."

  val InlineArgAssignOrigin: Origin = Origin(Seq(
    PreferredName(Seq("inlineArgAssign")),
    LabelContext("Assign of argument during inlining"),
  ))

  case class WrapperInlineFailed(inv: ProcedureInvocation[_], msg: String = "")
      extends SystemError {
    override def text: String = {
      Message.messagesInContext((
        inv.o,
        "Inlining of wrapper-function in pallas specification failed. " + msg,
      ))
    }
  }
}

case class InlinePallasWrappers[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(decl: Declaration[Pre]): Unit = {

    // Drop all definitions of wrapper functions, since they will be inlined.
    decl match {
      case proc: Procedure[Pre] if proc.pallasWrapper =>
      case other => rewriteDefault(other)
    }
  }

  override def dispatch(node: Expr[Pre]): Expr[Rewritten[Pre]] = {

    node match {
      case res: LLVMIntermediaryResult[Pre] =>
        implicit val o: Origin = res.o
        res.sretArg match {
          case Some(Ref(retArg)) => Local[Post](ref = succ(retArg))
          case None => Result[Post](applicable = succ(res.applicable.decl))
        }
      case old: LLVMOld[Pre] =>
        implicit val o: Origin = old.o
        Old[Post](Local(succ(old.v.decl)), None)(PanicBlame(
          "Cannot refer to invalid label."
        ))
      case inv: ProcedureInvocation[Pre] if inv.ref.decl.pallasWrapper =>
        // Inline pallas wrapper into their call-sites
        val wFunc = inv.ref.decl

        if (wFunc.body.isEmpty) {
          throw WrapperInlineFailed(inv, "Cannot inline function without body")
        }
        if (wFunc.args.size != inv.args.size) {
          throw WrapperInlineFailed(
            inv,
            "Number of arguments differs between definition and invocation.",
          )
        }

        // Declare variables to substitute the vars from the function definition.
        val newArgs = localHeapVariables.scope {
          variables.scope {
            wFunc.args.map(arg => new Variable[Pre](arg.t)(arg.o))
          }
        }
        // Assign the passed-in values from the call-site to the new variables.
        val assigns = newArgs.zip(inv.args).map { case (v, e) =>
          assignLocal(new Local[Pre](v.ref)(v.o), e)(InlineArgAssignOrigin)
        }
        // Substitute references to the original arguments of the function-definition with the new variables
        val varSubstitutions = wFunc.args.zip(newArgs)
        val substBody = Option(
          SubstituteReferences(varSubstitutions.toMap).dispatch(wFunc.body.get)
        )

        val bodyWithAssign = Block(assigns ++ substBody)(inv.o)
        val inlinedBody = StatementToExpression(
          this,
          (s: String) => WrapperInlineFailed(inv, s),
        ).toExpression(bodyWithAssign, None)

        inlinedBody match {
          case Some(e) => e
          case None =>
            throw WrapperInlineFailed(
              inv,
              "Wrapper could not be converted to expression.",
            )
        }
      case other => rewriteDefault(other)
    }

  }
}
