package vct.rewrite

import vct.col.ast.{
  AssignExpression,
  AssignStmt,
  Block,
  Eval,
  Expr,
  Local,
  LocalDecl,
  Loop,
  Scope,
  Statement,
}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

case object CanonicalizeLoops extends RewriterBuilder {

  override def key: String = "canonicalizeLoops"

  override def desc: String =
    "Detects some loop patterns to make them usable with iteration contracts"
}

case class CanonicalizeLoops[Pre <: Generation]() extends Rewriter[Pre] {

  private def getLastStat(
      body: Statement[Pre]
  ): (Option[Statement[Pre]], Statement[Pre]) =
    body match {
      case Block(Nil) => (None, body)
      case Block(s) =>
        val (last, remainder) = getLastStat(s.last)
        (last, Block(s.init :+ remainder)(body.o))
      case Scope(vars, inner) =>
        val (last, remainder) = getLastStat(inner);
        (last, Scope(vars, remainder)(body.o))
      case _ => (Some(body), Block(Nil)(body.o))
    }

  private def getAssignTarget(s: Statement[Pre]): Option[Expr[Pre]] =
    s match {
      case a: AssignStmt[Pre] => Some(a.target)
      case Eval(a: AssignExpression[Pre]) => Some(a.target)
      case _ => None
    }

  override def dispatch(s: Statement[Pre]): Statement[Post] =
    s match {
      case Block(
            Seq(
              LocalDecl(v0),
              init: Statement[Pre],
              s @ Scope(_, l @ Loop(Block(Nil), cond, Block(Nil), _, body)),
            )
          ) =>
        (getAssignTarget(init), getLastStat(body)) match {
          case (Some(Local(Ref(v1))), (Some(update: Statement[Pre]), remainder))
              if v1 == v0 =>
            getAssignTarget(update) match {
              case Some(Local(Ref(v2))) if v2 == v0 && cond.collectFirst {
                    case Local(Ref(v3)) if v3 == v0 =>
                  }.isDefined =>
                s.rewrite(
                  locals = variables.dispatch(s.locals :+ v0),
                  body = l.rewrite(
                    init = Block(Seq(dispatch(init)))(init.o),
                    update = Block(Seq(dispatch(update)))(update.o),
                    body = dispatch(remainder),
                  ),
                )
              case _ => super.dispatch(s)
            }
          case _ => super.dispatch(s)
        }
      case _ => super.dispatch(s)
    }

}
