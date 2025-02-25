package vct.col.util

import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.{Generation, NonLatchingRewriter}
import vct.result.VerificationError

case class StatementToExpression[Pre <: Generation, Post <: Generation](
    rw: NonLatchingRewriter[Pre, Post],
    errorBuilder: String => VerificationError,
) {

  def toExpression(
      stat: Statement[Pre],
      alt: => Option[Expr[Post]],
  ): Option[Expr[Post]] = {
    implicit val o: Origin = DiagnosticOrigin
    stat match {
      case Return(e) =>
        alt match {
          case Some(_) =>
            throw errorBuilder("Dead code after return is not allowed.")
          case None => Some(rw.dispatch(e))
        }
      case Block(Nil) => alt
      case Block(stat +: tail) =>
        toExpression(stat, toExpression(Block(tail), alt))
      case Branch(Nil) => alt
      case Branch((BooleanValue(true), impl) +: _) => toExpression(impl, alt)
      case Branch((cond, impl) +: branches) =>
        Some(Select(
          rw.dispatch(cond),
          toExpression(impl, alt).getOrElse(return None),
          toExpression(Branch(branches), alt).getOrElse(return None),
        ))
      case Scope(locals, impl) =>
        if (!locals.forall(countAssignments(_, impl).exists(_ <= 1))) {
          throw errorBuilder("Variables may only be assigned once.")
        }
        toExpression(impl, alt)
      case a @ Assign(Local(ref), e) =>
        rw.localHeapVariables.scope {
          rw.variables.scope {
            alt match {
              case Some(exprAlt) =>
                Some(
                  Let[Post](
                    rw.variables.collect { rw.dispatch(ref.decl) }._1.head,
                    rw.dispatch(e),
                    exprAlt,
                  )(a.o)
                )
              case None =>
                throw errorBuilder("Assign may not be the last statement")
            }
          }
        }
      case _ => None
    }
  }

  private def countAssignments[G <: Generation](
      v: Variable[G],
      s: Statement[G],
  ): Option[Int] =
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

}
