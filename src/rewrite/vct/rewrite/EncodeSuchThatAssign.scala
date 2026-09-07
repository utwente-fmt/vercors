package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{Substitute, SuccessionMap}
import vct.result.VerificationError.UserError

import scala.collection.mutable

object EncodeSuchThatAssign extends RewriterBuilder {

  override def key: String = "suchThatAssign"
  override def desc: String = "Encode such-that assignments and lets."

  private case class FailedAssignSuchThat(
      assign: Node[_],
      fault: Blame[AssignSuchThatFailed],
  ) extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit = {
      fault.blame(AssignSuchThatFailed(assign))
    }
  }

  private case class DisallowedAssignmentTargetForSuchThat(target: Expr[_])
      extends UserError {
    override def code: String = "disallowedAssignmentTargetForSuchThat"
    override def text: String =
      target.o.messageInContext(
        "An assign such that statement can only assign to local variables."
      )
  }

  private case class LetSuchThatOnlyInPure(target: Expr[_]) extends UserError {
    override def code: String = "letSuchThatOnlyInPure"
    override def text: String =
      target.o.messageInContext(
        "A let-such-that expression should only be used in pure function: use assign-such-that here instead."
      )
  }

}

case class EncodeSuchThatAssign[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeSuchThatAssign._

  private val nonDetMethods: mutable.Map[Type[Post], Function[Post]] = mutable
    .Map()
  private var nonDetNumber: BigInt = 0

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: AbstractMethod[Pre] if !p.pure =>
        p.body.map(_.collectFirst { case st: LetSuchThat[Pre] =>
          throw LetSuchThatOnlyInPure(st)
        })
      case _ =>
    }
    super.dispatch(decl)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case a @ AssignSuchThat(_, _) => rewriteAssignSuchThat(a)
      case _ => super.dispatch(stat)
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case a @ LetSuchThat(_, _, _) => rewriteLetSuchThat(a)
      case _ => super.dispatch(expr)
    }

  def rewriteLetSuchThat(ass: LetSuchThat[Pre]): Expr[Post] = {
    implicit val o: Origin = ass.o
    val LetSuchThat(target, constraint, body) = ass

    val targetNew = variables.collect(dispatch(target))._1.head
    val constraintNew = dispatch(constraint)
    val t = dispatch(target.t)

    val existsVar =
      variables.collect {
        val existsTarget =
          new Variable[Post](t)(
            target.o
              .where(name = target.o.getPreferredNameOrElse(Seq("x")).snake)
          )
        variables.declare(existsTarget)
      }._1.head

    val existsLocal = Local[Post](existsVar.ref)(target.o)
    val existsBody = Substitute[Post](Map(
      Local[Post](targetNew.ref) -> existsLocal
    )).dispatch(constraintNew)
    val nonDet: Function[Post] = nonDetMethods.getOrElseUpdate(t, makeNonDet(t))
    val nr = nonDetNumber
    nonDetNumber += 1

    val bodyNew = dispatch(body)
    val assume = Assuming(constraintNew, bodyNew)
    val letNew = Let(
      targetNew,
      functionInvocation[Post](TrueSatisfiable, nonDet.ref, Seq(const(nr))),
      assume,
    )
    val result =
      Asserting(Exists(Seq(existsVar), Seq(), existsBody), letNew)(
        FailedAssignSuchThat(ass, ass.blame)
//      PanicBlame("TODO")
      )
    result
  }

  def rewriteAssignSuchThat(ass: AssignSuchThat[Pre]): Statement[Post] = {
    implicit val o: Origin = ass.o
    val AssignSuchThat(target, constraint) = ass
    target match {
      case Local(_) =>
      case _ => throw DisallowedAssignmentTargetForSuchThat(target)
    }

    val targetNew = dispatch(target)
    val constraintNew = dispatch(constraint)
    val t = dispatch(target.t)

    val existsVar =
      variables.collect {
        val existsTarget =
          new Variable[Post](t)(
            target.o
              .where(name = target.o.getPreferredNameOrElse(Seq("x")).snake)
          )
        variables.declare(existsTarget)
      }._1.head
    val existsLocal = Local[Post](existsVar.ref)(target.o)
    val existsBody = Substitute(Map(targetNew -> existsLocal))
      .dispatch(constraintNew)
    val nonDet: Function[Post] = nonDetMethods.getOrElseUpdate(t, makeNonDet(t))
    val nr = nonDetNumber
    nonDetNumber += 1
    val assignNonDet =
      Assign(
        targetNew,
        functionInvocation[Post](TrueSatisfiable, nonDet.ref, Seq(const(nr))),
      )(AssignLocalOk)
    val checkExists =
      Assert(Exists(Seq(existsVar), Seq(), existsBody))(
        FailedAssignSuchThat(ass, ass.blame)
      )
    val assumeValue = Assume(constraintNew)
    Block[Post](Seq(assignNonDet, checkExists, assumeValue))
  }

  def makeNonDet(element: Type[Post]): Function[Post] = {
    implicit val o: Origin = Origin(
      Seq(LabelContext("non_det_" + element.toString))
    )

    globalDeclarations.declare({
      val (vars, _) = variables.collect {
        val a_var = new Variable[Post](TInt())(o.where(name = "p"))
        variables.declare(a_var)
      }

      function(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = element,
        args = vars,
        typeArgs = Nil,
        body = None,
        decreases = Some(DecreasesClauseNoRecursion[Post]()),
      )(o.where(name = "non_det_" + element.toString))
    })
  }
}
