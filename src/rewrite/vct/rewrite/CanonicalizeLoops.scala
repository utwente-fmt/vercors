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
  Variable,
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

  private def getVarsInLoop(s: Statement[Pre]): Set[Variable[Pre]] =
    s.collect {
      case Scope(vars, _) => vars.toSet
      case LocalDecl(v) => Set(v)
    }.fold(Set.empty)((l, r) => l ++ r)

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case b @ Block(s) =>
        var statements = s.map(dispatch)
        s.zipWithIndex.sliding(3).foreach {
          case Seq(
                (LocalDecl(v0), start),
                (init: Statement[Pre], _),
                (
                  s @ Scope(_, l @ Loop(Block(Nil), cond, Block(Nil), _, body)),
                  _,
                ),
              ) =>
            val innerVars = getVarsInLoop(s)
            (getAssignTarget(init), getLastStat(body)) match {
              case (
                    Some(Local(Ref(v1))),
                    (Some(update: Statement[Pre]), remainder),
                  ) if v1 == v0 && !update.exists {
                    case Local(Ref(v2)) if innerVars.contains(v2) => true
                  } =>
                getAssignTarget(update) match {
                  case Some(Local(Ref(v2))) if v2 == v0 && cond.collectFirst {
                        case Local(Ref(v3)) if v3 == v0 =>
                      }.isDefined =>
                    statements =
                      (statements.take(start + 1) :+ s.rewrite(
                        locals = variables.dispatch(s.locals),
                        body = l.rewrite(
                          init = Block(Seq(dispatch(init)))(init.o),
                          update = Block(Seq(dispatch(update)))(update.o),
                          body = dispatch(remainder),
                        ),
                      )) ++ statements.drop(start + 3)
                  case _ =>
                }
              case _ =>
            }
          case _ =>

        }
        b.rewrite(statements = statements)
      case _ => super.dispatch(stat)
    }

}
