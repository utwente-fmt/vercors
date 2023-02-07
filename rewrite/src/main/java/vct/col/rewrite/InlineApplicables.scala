package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{AssertFailed, Blame, FoldFailed, Origin, UnfoldFailed}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute
import vct.result.VerificationError.{Unreachable, UserError}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

case object InlineApplicables extends RewriterBuilder {
  override def key: String = "inline"
  override def desc: String = "Inline applicables into their usage sites for applicables marked inline."

  case class CyclicInline(applications: Seq[Apply[_]]) extends UserError {
    override def code: String = "cyclicInline"
    override def text: String =
      applications match {
        case Seq(app) => app.o.messageInContext("This application cannot be inlined, since the applicable refers to itself.")
        case first :: more =>
          Origin.messagesInContext(
            (first.o, "This application cannot be inlined, since it requires inlining ...") +:
              more.map(apply => (apply.o, "... this application, which requires inlining ...")) :+
              (first.o, "... this application: a cycle.")
          )
      }
  }

  case class AbstractInlineable(use: Apply[_], inlineable: InlineableApplicable[_]) extends UserError {
    override def code: String = "abstractInlined"
    override def text: String =
      Origin.messagesInContext(Seq(
        use.o -> "This application cannot be inlined, since ...",
        inlineable.o -> "... the definition is abstract.",
      ))
  }

  case class ReplaceReturn[G](newStatement: Expr[G] => Statement[G]) extends NonLatchingRewriter[G, G] {
    case object IdentitySuccessor extends SuccessorsProviderTrafo(allScopes.freeze) {
      override def preTransform[I <: Declaration[G], O <: Declaration[G]](pre: I): Option[O] =
        Some(pre.asInstanceOf[O])
    }

    override def succProvider: SuccessorsProvider[G, G] = IdentitySuccessor

    override def dispatch(stat: Statement[G]): Statement[G] = stat match {
      case Return(e) => newStatement(e)
      case other => rewriteDefault(other)
    }
  }

  case class InlinedOrigin(definition: Origin, usages: Seq[Apply[_]]) extends Origin {
    override def preferredName: String = definition.preferredName
    override def shortPosition: String = usages.head.o.shortPosition
    override def context: String =
      usages.map(_.o.context).mkString(
        start = " Inlined from:\n" + Origin.HR,
        sep = Origin.HR + " ...Then inlined from:\n" + Origin.HR,
        end = "",
      ) + Origin.HR +
        " In definition:\n" + Origin.HR +
        definition.context

    override def inlineContext: String = s"${definition.inlineContext} [inlined from] ${usages.head.o.inlineContext}"
  }

  case class InlineFoldAssertFailed(fold: Fold[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      fold.blame.blame(FoldFailed(error.failure, fold))
  }

  case class InlineUnfoldAssertFailed(unfold: Unfold[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      unfold.blame.blame(UnfoldFailed(error.failure, unfold))
  }

  case class Replacement[Pre](replacing: Expr[Pre], binding: Expr[Pre])(implicit o: Origin) {
    val withVariable: Variable[Pre] = new Variable(replacing.t)
    def +(other: Replacements[Pre]): Replacements[Pre] = Replacements(Seq(this)) + other
    def +(other: Replacement[Pre]): Replacements[Pre] = Replacements(Seq(this)) + other
  }
  case class Replacements[Pre](replacements: Seq[Replacement[Pre]]) {
    def +(other: Replacements[Pre]): Replacements[Pre] = Replacements(replacements ++ other.replacements)
    def +(other: Replacement[Pre]): Replacements[Pre] = Replacements(replacements :+ other)

    def expr(e: Expr[Pre])(implicit o: Origin): Expr[Pre] = {
      val replaced = Substitute[Pre](replacements.map(r => r.replacing -> r.withVariable.get).toMap).dispatch(e)
      replacements.foldRight(replaced) {
        case (replacement, e) => Let(replacement.withVariable, replacement.binding, e)(e.o)
      }
    }

    def captureReturn(t: Type[Pre], body: Statement[Pre])(implicit o: Origin): Expr[Pre] = {
      val done = Label[Pre](new LabelDecl(), Block(Nil))
      val result = new Variable[Pre](t)
      val newBody = ReplaceReturn((e: Expr[Pre]) => Block(Seq(assignLocal(result.get, e), Goto[Pre](done.decl.ref)))).dispatch(body)
      ScopedExpr(Seq(result), With(Block(Seq(newBody, done)), result.get))
    }

    def stat(t: Type[Pre], s: Statement[Pre], outReplacements: Replacements[Pre])(implicit o: Origin): Expr[Pre] = {
      val replaced = Substitute[Pre]((this + outReplacements).replacements.map(r => r.replacing -> r.withVariable.get).toMap).dispatch(s)
      val capture = captureReturn(t, replaced)
      ScopedExpr((this + outReplacements).replacements.map(_.withVariable),
        With(Block(
          replacements.map(r => assignLocal(r.withVariable.get, r.binding))
        ), Then(
          capture, Block(
            outReplacements.replacements.map(r => Assign(r.binding, r.withVariable.get)(null))
        )))
      )
    }
  }
}

case class InlineApplicables[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  import InlineApplicables._

  val inlineStack: ScopedStack[Apply[Pre]] = ScopedStack()
  val classOwner: mutable.Map[ClassDeclaration[Pre], Class[Pre]] = mutable.Map()

  override def dispatch(o: Origin): Origin =
    inlineStack.toSeq match {
      case Nil => o
      case some => InlinedOrigin(o, some.reverse)
    }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.declarations.collect { case cls: Class[Pre] => cls }.foreach { cls =>
      cls.declarations.foreach(classOwner(_) = cls)
    }

    rewriteDefault(program)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case app: InlineableApplicable[Pre] if app.inline =>
      app.drop()
    case other => rewriteDefault(other)
  }

  @tailrec
  private def isInlinePredicateApply(e: Expr[Pre]): Boolean = e match {
    case PredicateApply(Ref(pred), _, _) => pred.inline
    case InstancePredicateApply(_, Ref(pred), _, _) => pred.inline
    case Scale(_, res) => isInlinePredicateApply(res)
    case _ => false
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case f @ Fold(e) if isInlinePredicateApply(e) => Assert(dispatch(e))(InlineFoldAssertFailed(f))(stat.o)
    case u @ Unfold(e) if isInlinePredicateApply(e) => Assert(dispatch(e))(InlineUnfoldAssertFailed(u))(stat.o)

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case apply: ApplyInlineable[Pre] if apply.ref.decl.inline =>
      implicit val o: Origin = apply.o

      // Some fanfare here to produce a nice diagnostic when the cycle of applications is large.
      // First reverse the stack of to-be-inlined applications, since toSeq of a stack presents the head first.
      // Next skip any declarations that are not equal to the declaration of the current apply: they are unrelated to the cycle.
      // Finally throw if we have found an apply that has the same declaration as us, but skip that initial apply:
      // it may not be part of the real cycle (but just the entry into it).
      inlineStack.toSeq.reverse.dropWhile(_.ref.decl != apply.ref.decl) match {
        case Nil => // ok
        case some => throw CyclicInline(some.tail :+ apply)
      }

      inlineStack.having(apply) {
        lazy val obj = {
          val instanceApply = apply.asInstanceOf[InstanceApply[Pre]]
          val cls = classOwner(instanceApply.ref.decl)
          Replacement(ThisObject[Pre](cls.ref), instanceApply.obj)
        }

        lazy val args =
          Replacements(for((arg, v) <- apply.args.zip(apply.ref.decl.args))
            yield Replacement(v.get, arg))

        lazy val givenArgs =
          Replacements(for((Ref(v), arg) <- apply.asInstanceOf[InvokingNode[Pre]].givenMap)
            yield Replacement(v.get, arg))

        lazy val outArgs = {
          val inv = apply.asInstanceOf[AnyMethodInvocation[Pre]]
          Replacements(for((out, v) <- inv.outArgs.zip(inv.ref.decl.outArgs))
            yield Replacement(v.get, out))
        }

        lazy val yields =
          Replacements(for((out, Ref(v)) <- apply.asInstanceOf[InvokingNode[Pre]].yields)
            yield Replacement(v.get, out))

        val replacements = apply.ref.decl.args.map(_.get).zip(apply.args).toMap[Expr[Pre], Expr[Pre]]
        // TODO: consider type arguments and out-arguments and given and yields (oof)
        apply match {
          case PredicateApply(Ref(pred), _, WritePerm()) => // TODO inline predicates with non-write perm
            dispatch(args.expr(pred.body.getOrElse(throw AbstractInlineable(apply, pred))))
          case PredicateApply(Ref(pred), _, _) => ???
          case ProcedureInvocation(Ref(proc), _, _, typeArgs, _, _) =>
            dispatch((args + givenArgs).stat(apply.t, proc.body.getOrElse(throw AbstractInlineable(apply, proc)), outArgs + yields))
          case FunctionInvocation(Ref(func), _, typeArgs, _, yields) =>
            dispatch((args + givenArgs).expr(func.body.getOrElse(throw AbstractInlineable(apply, func))))

          case MethodInvocation(_, Ref(method), _, _, typeArgs, _, _) =>
            dispatch((obj + args + givenArgs).stat(apply.t, method.body.getOrElse(throw AbstractInlineable(apply, method)), outArgs + yields))
          case InstanceFunctionInvocation(_, Ref(func), _, typeArgs, _, yields) =>
            dispatch((obj + args + givenArgs).expr(func.body.getOrElse(throw AbstractInlineable(apply, func))))
          case InstancePredicateApply(_, Ref(pred), _, WritePerm()) =>
            dispatch((obj + args).expr(pred.body.getOrElse(throw AbstractInlineable(apply, pred))))
          case InstancePredicateApply(_, Ref(pred), _, _) => ???
        }
      }

    case Unfolding(PredicateApply(Ref(pred), args, perm), body) if pred.inline =>
      With(Block(args.map(dispatch).map(e => Eval(e)(e.o)) :+ Eval(dispatch(perm))(perm.o))(e.o), dispatch(body))(e.o)

    case Unfolding(InstancePredicateApply(obj, Ref(pred), args, perm), body) if pred.inline =>
      With(Block(
        Seq(Eval(dispatch(obj))(obj.o)) ++
          args.map(dispatch).map(e => Eval(e)(e.o)) ++
          Seq(Eval(dispatch(perm))(perm.o))
      )(e.o), dispatch(body))(e.o)

    case other => rewriteDefault(other)
  }
}
