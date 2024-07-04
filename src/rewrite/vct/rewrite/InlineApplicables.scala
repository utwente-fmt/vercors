package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{
  AssertFailed,
  Blame,
  FoldFailed,
  LabelContext,
  Origin,
  PanicBlame,
  PreferredName,
  UnfoldFailed,
}
import vct.col.ref.Ref
import vct.col.rewrite.error.ExcludedByPassOrder
import vct.col.rewrite.{
  Generation,
  NonLatchingRewriter,
  Rewriter,
  RewriterBuilder,
  Rewritten,
}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Substitute
import vct.result.Message
import vct.result.VerificationError.{Unreachable, UserError}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

case object InlineApplicables extends RewriterBuilder {
  override def key: String = "inline"
  override def desc: String =
    "Inline applicables into their usage sites for applicables marked inline."

  case class CyclicInline(applications: Seq[Node[_]]) extends UserError {
    override def code: String = "cyclicInline"
    override def text: String =
      applications match {
        case Seq(app) =>
          app.o.messageInContext(
            "This application cannot be inlined, since the applicable refers to itself."
          )
        case first +: more =>
          Message.messagesInContext(
            (
              first.o,
              "This application cannot be inlined, since it requires inlining ...",
            ) +: more.map(apply =>
              (apply.o, "... this application, which requires inlining ...")
            ) :+ (first.o, "... this application: a cycle."): _*
          )
      }
  }

  case class AbstractInlineable(use: Node[_], inlineable: Node[_])
      extends UserError {
    override def code: String = "abstractInlined"
    override def text: String =
      Message.messagesInContext(
        use.o -> "This application cannot be inlined, since ...",
        inlineable.o -> "... the definition is abstract.",
      )
  }

  case class WrongPredicateLocation(use: Location[_]) extends UserError {
    override def code: String = "wrongInlinePredicate"
    override def text: String =
      use.o.messageInContext(
        "This location refers to an inline predicate, but it cannot be inlined in this position."
      )
  }

  case class ReplaceReturn[G](newStatement: Expr[G] => Statement[G])
      extends NonLatchingRewriter[G, G] {
    case class SuccOrIdentity()
        extends SuccessorsProviderTrafo[G, G](allScopes) {
      override def postTransform[T <: Declaration[G]](
          pre: Declaration[G],
          post: Option[T],
      ): Option[T] = Some(post.getOrElse(pre.asInstanceOf[T]))
    }

    override def succProvider: SuccessorsProvider[G, G] = SuccOrIdentity()

    override def dispatch(stat: Statement[G]): Statement[G] =
      stat match {
        case Return(e) => newStatement(dispatch(e))
        case other => rewriteDefault(other)
      }
  }

  private def InlinedOrigin(definition: Origin, usages: Seq[Node[_]]): Origin =
    Origin(
      usages.flatMap(usage =>
        LabelContext("inlined from") +: usage.o.originContents
      ) ++ (LabelContext("definition") +: definition.originContents)
    )

  private def InlineLetThisOrigin: Origin =
    Origin(Seq(PreferredName(Seq("self")), LabelContext("this")))

  case class InlineFoldAssertFailed(fold: Fold[_]) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      fold.blame.blame(FoldFailed(error.failure, fold))
  }

  case class InlineUnfoldAssertFailed(unfold: Unfold[_])
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      unfold.blame.blame(UnfoldFailed(error.failure, unfold))
  }

  case class Replacement[Pre](replacing: Expr[Pre], binding: Expr[Pre])(
      implicit o: Origin
  ) {
    val withVariable: Variable[Pre] = new Variable(replacing.t)
    def +(other: Replacements[Pre]): Replacements[Pre] =
      Replacements(Seq(this)) + other
    def +(other: Replacement[Pre]): Replacements[Pre] =
      Replacements(Seq(this)) + other
  }
  case class Replacements[Pre](replacements: Seq[Replacement[Pre]]) {
    def +(other: Replacements[Pre]): Replacements[Pre] =
      Replacements(replacements ++ other.replacements)
    def +(other: Replacement[Pre]): Replacements[Pre] =
      Replacements(replacements :+ other)

    def expr(e: Expr[Pre])(implicit o: Origin): Expr[Pre] = {
      val sub = Substitute[Pre](
        replacements.map(r => r.replacing -> r.withVariable.get).toMap
      )
      val replaced = sub.labelDecls.scope { sub.dispatch(e) }
      replacements.foldRight(replaced) { case (replacement, e) =>
        Let(replacement.withVariable, replacement.binding, e)(e.o)
      }
    }

    def captureReturn(t: Type[Pre], body: Statement[Pre])(
        implicit o: Origin
    ): Expr[Pre] = {
      val done = Label[Pre](new LabelDecl(), Block(Nil))
      val result = new Variable[Pre](t)
      val sub = ReplaceReturn((e: Expr[Pre]) =>
        Block(Seq(assignLocal(result.get, e), Goto[Pre](done.decl.ref)))
      )
      val newBody = sub.labelDecls.scope { sub.dispatch(body) }
      ScopedExpr(Seq(result), With(Block(Seq(newBody, done)), result.get))
    }

    def stat(
        t: Type[Pre],
        s: Statement[Pre],
        outReplacements: Replacements[Pre],
    )(implicit o: Origin): Expr[Pre] = {
      val sub = Substitute[Pre](
        (this + outReplacements).replacements
          .map(r => r.replacing -> r.withVariable.get).toMap
      )
      // labelDecls is the only scope peeled off by taking the body of the to-be-inlined applicable.
      val replaced = sub.labelDecls.scope { sub.dispatch(s) }
      val capture = captureReturn(t, replaced)
      ScopedExpr(
        (this + outReplacements).replacements.map(_.withVariable),
        With(
          Block(
            replacements.map(r => assignLocal(r.withVariable.get, r.binding))
          ),
          Then(
            capture,
            Block(
              outReplacements.replacements
                .map(r => Assign(r.binding, r.withVariable.get)(null))
            ),
          ),
        ),
      )
    }
  }
}

case class InlineApplicables[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  import InlineApplicables._

  val inlineStack: ScopedStack[(Node[Pre], Declaration[Pre])] = ScopedStack()
  val classOwner: mutable.Map[ClassDeclaration[Pre], Class[Pre]] = mutable.Map()

  override def dispatch(o: Origin): Origin =
    inlineStack.toSeq match {
      case Nil => o
      case some => InlinedOrigin(o, some.reverse.map(_._1))
    }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.declarations.collect { case cls: Class[Pre] => cls }.foreach {
      cls => cls.decls.foreach(classOwner(_) = cls)
    }
    program.rewriteDefault()
  }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case app: InlineableApplicable[Pre] if app.inline => app.drop()
      case other => allScopes.anySucceed(other, other.rewriteDefault())
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case f @ Fold(target @ ScaledPredicateApply(inv, perm))
          if inv.ref.decl.inline =>
        Assert(permExpression(PredicateLocation(inv)(f.o), perm, target.o))(
          InlineFoldAssertFailed(f)
        )(stat.o)
      case u @ Unfold(target @ ScaledPredicateApply(inv, perm))
          if inv.ref.decl.inline =>
        Assert(permExpression(PredicateLocation(inv)(u.o), perm, target.o))(
          InlineUnfoldAssertFailed(u)
        )(stat.o)

      case other => other.rewriteDefault()
    }

  override def dispatch(loc: Location[Pre]): Location[Post] =
    loc match {
      case loc @ PredicateLocation(inv) if inv.ref.decl.inline =>
        throw WrongPredicateLocation(loc)

      case other => other.rewriteDefault()
    }

  def checkCycle[T](use: Node[Pre], decl: Declaration[Pre])(f: => T): T = {
    // Some fanfare here to produce a nice diagnostic when the cycle of applications is large.
    // First reverse the stack of to-be-inlined applications, since toSeq of a stack presents the head first.
    // Next skip any declarations that are not equal to the declaration of the current apply: they are unrelated to the cycle.
    // Finally throw if we have found an apply that has the same declaration as us, but skip that initial apply:
    // it may not be part of the real cycle (but just the entry into it).
    inlineStack.toSeq.reverse.dropWhile(_._2 != decl) match {
      case Nil => // ok
      case some => throw CyclicInline(some.tail.map(_._1) :+ use)
    }

    inlineStack.having(use -> decl) { f }
  }

  def permExpression(
      loc: Location[Pre],
      perm: Expr[Pre],
      o: Origin,
  ): Expr[Post] =
    loc match {
      case InLinePatternLocation(loc, trigExpr) =>
        Perm(
          InLinePatternLocation(dispatch(loc), dispatch(trigExpr))(dispatch(
            loc.o
          )),
          dispatch(perm),
        )(dispatch(o))
      case PredicateLocation(inv) if inv.ref.decl.inline =>
        checkCycle(inv, inv.ref.decl) {
          implicit val replOrigin: Origin = inv.o
          lazy val obj = {
            val instanceApply = inv.asInstanceOf[InstancePredicateApply[Pre]]
            val cls = classOwner(instanceApply.ref.decl)
            Replacement(ThisObject[Pre](cls.ref), instanceApply.obj)(
              InlineLetThisOrigin
            )
          }

          lazy val args = Replacements(
            for ((arg, v) <- inv.args.zip(inv.ref.decl.args))
              yield Replacement(v.get, arg)(v.o)
          )

          val inlinedBody =
            inv match {
              case PredicateApply(Ref(pred), _) =>
                dispatch(args.expr(
                  pred.body.getOrElse(throw AbstractInlineable(inv, pred))
                ))

              case InstancePredicateApply(_, Ref(pred), _) =>
                dispatch((obj + args).expr(
                  pred.body.getOrElse(throw AbstractInlineable(inv, pred))
                ))

              case coalesce: CoalesceInstancePredicateApply[Pre] =>
                throw ExcludedByPassOrder(
                  "No more coalescing predicate applications here",
                  Some(coalesce),
                )
            }

          Scale(dispatch(perm), inlinedBody)(PanicBlame("Bug #1012"))
        }
      case other => Perm(dispatch(other), dispatch(perm))(dispatch(o))
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case apply: ApplyInlineable[Pre] if apply.ref.decl.inline =>
        implicit val o: Origin = apply.o

        checkCycle(apply, apply.ref.decl) {
          lazy val obj = {
            val instanceApply = apply.asInstanceOf[InstanceApply[Pre]]
            val cls = classOwner(instanceApply.ref.decl)
            Replacement(ThisObject[Pre](cls.ref), instanceApply.obj)(
              InlineLetThisOrigin
            )
          }

          lazy val args = Replacements(
            for ((arg, v) <- apply.args.zip(apply.ref.decl.args))
              yield Replacement(v.get, arg)(v.o)
          )

          lazy val givenArgs = Replacements(
            for (
              (Ref(v), arg) <- apply.asInstanceOf[InvokingNode[Pre]].givenMap
            )
              yield Replacement(v.get, arg)(v.o)
          )

          lazy val outArgs = {
            val inv = apply.asInstanceOf[AnyMethodInvocation[Pre]]
            Replacements(
              for ((out, v) <- inv.outArgs.zip(inv.ref.decl.outArgs))
                yield Replacement(v.get, out)(v.o)
            )
          }

          lazy val yields = Replacements(
            for ((out, Ref(v)) <- apply.asInstanceOf[InvokingNode[Pre]].yields)
              yield Replacement(v.get, out)(v.o)
          )

          // TODO: consider type arguments
          apply match {
            case ProcedureInvocation(Ref(proc), _, _, typeArgs, _, _) =>
              dispatch((args + givenArgs).stat(
                apply.t,
                proc.body.getOrElse(throw AbstractInlineable(apply, proc)),
                outArgs + yields,
              ))
            case FunctionInvocation(Ref(func), _, typeArgs, _, _) =>
              dispatch((args + givenArgs).expr(
                func.body.getOrElse(throw AbstractInlineable(apply, func))
              ))

            case MethodInvocation(_, Ref(method), _, _, typeArgs, _, _) =>
              dispatch((obj + args + givenArgs).stat(
                apply.t,
                method.body.getOrElse(throw AbstractInlineable(apply, method)),
                outArgs + yields,
              ))
            case InstanceFunctionInvocation(_, Ref(func), _, typeArgs, _, _) =>
              dispatch((obj + args + givenArgs).expr(func.body.getOrElse(
                throw AbstractInlineable(apply, func)
              )))
          }
        }

      case Unfolding(
            ScaledPredicateApply(PredicateApply(Ref(pred), args), perm),
            body,
          ) if pred.inline =>
        With(
          Block(
            args.map(dispatch).map(e => Eval(e)(e.o)) :+
              Eval(dispatch(perm))(perm.o)
          )(e.o),
          dispatch(body),
        )(e.o)

      case Unfolding(
            ScaledPredicateApply(
              InstancePredicateApply(obj, Ref(pred), args),
              perm,
            ),
            body,
          ) if pred.inline =>
        With(
          Block(
            Seq(Eval(dispatch(obj))(obj.o)) ++ args.map(dispatch)
              .map(e => Eval(e)(e.o)) ++ Seq(Eval(dispatch(perm))(perm.o))
          )(e.o),
          dispatch(body),
        )(e.o)

      case Perm(loc, perm) => permExpression(loc, perm, e.o)

      case other => other.rewriteDefault()
    }
}
