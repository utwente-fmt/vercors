package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.util.Substitute
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.{Unreachable, UserError}

case object InlineApplicables {
  case class CyclicInline(applications: Seq[Apply]) extends UserError {
    override def code: String = "cyclicInline"
    override def text: String = ""
  }

  case class ReplaceReturn(newStatement: Expr => Statement) extends Rewriter {
    override def dispatch(stat: Statement): Statement = stat match {
      case Return(e) => newStatement(e)
      case other => rewriteDefault(other)
    }
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
        val replacements = apply.ref.decl.args.map(_.get).zip(apply.args).toMap[Expr, Expr]
        // TODO: consider type arguments and out-arguments
        apply match {
          case ADTFunctionInvocation(typeArgs, ref, args) =>
            throw Unreachable("ADT functions are never inline.")

          case PredicateApply(Ref(pred), _) =>
            dispatch(Substitute(replacements).dispatch(pred.body.getOrElse(???)))
          case ProcedureInvocation(Ref(proc), _, outArgs, typeArgs) =>
            val done = Label(new LabelDecl(), Block(Nil))
            val v = new Variable(proc.returnType)
            val returnReplacement = (result: Expr) => Block(Seq(Assign(v.get, result), Goto(done.decl.ref)))
            val replacedArgumentsBody = Substitute(replacements).dispatch(proc.body.getOrElse(???))
            val body = ReplaceReturn(returnReplacement).dispatch(replacedArgumentsBody)
            With(Block(Seq(body, done)), v.get)
          case FunctionInvocation(Ref(func), _, typeArgs) =>
            dispatch(Substitute(replacements).dispatch(func.body.getOrElse(???)))

          case MethodInvocation(obj, Ref(method), _, outArgs, typeArgs) =>
            val done = Label(new LabelDecl(), Block(Nil))
            val v = new Variable(method.returnType)
            val replacementsWithObj = replacements ++ Map(AmbiguousThis() -> obj)
            val returnReplacement = (result: Expr) => Block(Seq(Assign(v.get, result), Goto(done.decl.ref)))
            val replacedArgumentsObjBody = Substitute(replacementsWithObj).dispatch(method.body.getOrElse(???))
            val body = ReplaceReturn(returnReplacement).dispatch(replacedArgumentsObjBody)
            With(Block(Seq(body, done)), v.get)
          case InstanceFunctionInvocation(obj, Ref(func), _, typeArgs) =>
            val replacementsWithObj = replacements ++ Map(AmbiguousThis() -> obj)
            dispatch(Substitute(replacementsWithObj).dispatch(func.body.getOrElse(???)))
          case InstancePredicateApply(obj, Ref(pred), _) =>
            val replacementsWithObj = replacements ++ Map(AmbiguousThis() -> obj)
            dispatch(Substitute(replacementsWithObj).dispatch(pred.body.getOrElse(???)))
        }
      }

    case other => rewriteDefault(other)
  }
}
