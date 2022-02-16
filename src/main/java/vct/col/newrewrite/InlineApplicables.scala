package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.newrewrite.util.Substitute
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.{Unreachable, UserError}

import scala.reflect.ClassTag

case object InlineApplicables extends RewriterBuilder {
  case class CyclicInline(applications: Seq[Apply[_]]) extends UserError {
    override def code: String = "cyclicInline"
    override def text: String = ""
  }

  case class ReplaceReturn[G](newStatement: Expr[G] => Statement[G]) extends NonLatchingRewriter[G, G] {
    override def lookupSuccessor: Declaration[G] => Option[Declaration[G]] = Some(_)

    override def dispatch(stat: Statement[G]): Statement[G] = stat match {
      case Return(e) => newStatement(e)
      case other => rewriteDefault(other)
    }
  }
}

case class InlineApplicables[Pre <: Generation]() extends Rewriter[Pre] {
  import InlineApplicables._

  val inlineStack: ScopedStack[Apply[Pre]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: InlineableApplicable[Pre] if app.inline =>
      app.drop()
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case apply: ApplyInlineable[Pre] if apply.ref.decl.inline =>
      implicit val o: Origin = apply.o

      if(inlineStack.exists(_.ref.decl == apply.ref.decl))
        throw CyclicInline(inlineStack.toSeq)

      inlineStack.having(apply) {
        val replacements = apply.ref.decl.args.map(_.get).zip(apply.args).toMap[Expr[Pre], Expr[Pre]]
        // TODO: consider type arguments and out-arguments and given and yields (oof)
        apply match {
          case PredicateApply(Ref(pred), _, WritePerm()) => // TODO inline predicates with non-write perm
            dispatch(Substitute(replacements).dispatch(pred.body.getOrElse(???)))
          case PredicateApply(Ref(pred), _, _) => ???
          case ProcedureInvocation(Ref(proc), _, outArgs, typeArgs, givenMap, yields) =>
            val done = Label[Pre](new LabelDecl(), Block(Nil))
            val v = new Variable[Pre](proc.returnType)
            val returnReplacement = (result: Expr[Pre]) => Block(Seq(assignLocal(v.get, result), Goto[Pre](done.decl.ref)))
            val replacedArgumentsBody = Substitute(replacements).dispatch(proc.body.getOrElse(???))
            val body = ReplaceReturn(returnReplacement).dispatch(replacedArgumentsBody)
            dispatch(With(Block(Seq(body, done)), v.get))
          case FunctionInvocation(Ref(func), _, typeArgs, givenMap, yields) =>
            dispatch(Substitute(replacements).dispatch(func.body.getOrElse(???)))

          case MethodInvocation(obj, Ref(method), _, outArgs, typeArgs, givenMap, yields) =>
            val done = Label[Pre](new LabelDecl(), Block(Nil))
            val v = new Variable[Pre](method.returnType)
            val replacementsWithObj = replacements ++ Map[Expr[Pre], Expr[Pre]](AmbiguousThis[Pre]() -> obj)
            val returnReplacement = (result: Expr[Pre]) => Block(Seq(assignLocal(v.get, result), Goto[Pre](done.decl.ref)))
            val replacedArgumentsObjBody = Substitute[Pre](replacementsWithObj).dispatch(method.body.getOrElse(???))
            val body = ReplaceReturn(returnReplacement).dispatch(replacedArgumentsObjBody)
            dispatch(With(Block(Seq(body, done)), v.get))
          case InstanceFunctionInvocation(obj, Ref(func), _, typeArgs, givenMap, yields) =>
            val replacementsWithObj = replacements ++ Map(AmbiguousThis[Pre]() -> obj)
            dispatch(Substitute(replacementsWithObj).dispatch(func.body.getOrElse(???)))
          case InstancePredicateApply(obj, Ref(pred), _, WritePerm()) =>
            val replacementsWithObj = replacements ++ Map(AmbiguousThis[Pre]() -> obj)
            dispatch(Substitute(replacementsWithObj).dispatch(pred.body.getOrElse(???)))
          case InstancePredicateApply(obj, Ref(pred), _, _) => ???
        }
      }

    case other => rewriteDefault(other)
  }
}
