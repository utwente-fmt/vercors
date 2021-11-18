package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.{ADTDeclaration, ADTFunction, ADTFunctionInvocation, AbstractPredicate, Applicable, Apply, ClassDeclaration, ContractApplicable, Declaration, Expr, ExtraDeclarationKind, FunctionInvocation, GlobalDeclaration, InstanceFunctionInvocation, InstancePredicate, InstancePredicateApply, Invocation, LabelDecl, MethodInvocation, ModelAction, ModelDeclaration, ModelProcess, ParBlockDecl, ParInvariantDecl, Predicate, PredicateApply, ProcedureInvocation, Ref, Variable}
import vct.col.newrewrite.util.Substitute
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.UserError

case object InlineApplicables {
  case class CyclicInline(applications: Seq[Apply]) extends UserError {
    override def code: String = "cyclicInline"
    override def text: String = ""
  }
}

case class InlineApplicables() extends Rewriter {
  import InlineApplicables._

  val inlineStack: ScopedStack[Apply] = ScopedStack()

  override def dispatch(decl: Declaration): Unit = decl match {
    case app: Applicable if app.inline =>
      // discard definition
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = e match {
    case apply: Apply if apply.ref.decl.inline =>
      implicit val o: Origin = apply.o

      if(inlineStack.exists(_.ref.decl == apply.ref.decl))
        throw CyclicInline(inlineStack.toSeq)

      inlineStack.having(apply) {
        apply match {
          case ADTFunctionInvocation(typeArgs, ref, args) =>

          case PredicateApply(Ref(pred), args) =>
          case ProcedureInvocation(Ref(proc), args, outArgs, typeArgs) =>
          case FunctionInvocation(Ref(func), args, typeArgs) =>
            val replacements = func.args.map(_.get).zip(args).toMap[Expr, Expr]
            dispatch(Substitute(replacements).dispatch(func.body.getOrElse(???)))

          case MethodInvocation(obj, Ref(method), args, outArgs, typeArgs) =>
          case InstanceFunctionInvocation(obj, Ref(func), args, typeArgs) =>
          case InstancePredicateApply(obj, Ref(pred), args) =>
        }
      }
  }
}
