package vct.col.ast.unsorted

import vct.col.ast.ops.ChorStatementOps
import vct.col.ast.statement.StatementImpl
import vct.col.ast.{
  Assign,
  Branch,
  ChorStatement,
  Communicate,
  Endpoint,
  EndpointExpr,
  EndpointStatement,
  Expr,
  Loop,
  Statement,
}
import vct.col.check.{CheckContext, CheckError, SeqProgParticipant}
import vct.col.print._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers

import scala.collection.immutable.ListSet
import scala.util.Try

trait ChorStatementImpl[G] extends ChorStatementOps[G] with StatementImpl[G] {
  this: ChorStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("/* choreographic statement */") <+/> inner

  def cond: Expr[G] =
    inner match {
      case branch: Branch[G] => branch.branches.head._1
      case loop: Loop[G] => loop.cond
    }

  def guards: Seq[Expr[G]] = AstBuildHelpers.unfoldStar(cond)

  def explicitEndpoints: Seq[Endpoint[G]] =
    guards.collect { case EndpointExpr(Ref(endpoint), _) => endpoint }

  def hasUnpointed: Boolean =
    guards.exists {
      case _: EndpointExpr[G] => false
      case _ => true
    }

  object branch {
    def apply(): Branch[G] = inner.asInstanceOf[Branch[G]]

    def yes: Statement[G] = branch().branches.head._2
    def no: Option[Statement[G]] = Try(branch().branches(1)).toOption.map(_._2)
  }

  object loop {
    def apply(): Loop[G] = inner.asInstanceOf[Loop[G]]
  }

  def branches: Boolean =
    inner match {
      case _: Branch[G] | _: Loop[G] => true
      case _ => false
    }

  def participants: Set[Endpoint[G]] =
    ListSet.from(collect {
      case comm: Communicate[G] => comm.participants
      case EndpointStatement(Some(Ref(endpoint)), Assign(_, _)) => Seq(endpoint)
      case c @ ChorStatement(_) => c.explicitEndpoints
    }.flatten)

  // There are only a few statements where we fully define how projection works - for now
  def allowedInner: Option[CheckError] =
    Option.unless(branches)(vct.col.check.ChorStatement(this))

  def participantCheck(context: CheckContext[G]): Option[CheckError] =
    // There are participants in this if that have been excluded from participation: error
    Option.unless(
      Set.from(explicitEndpoints)
        .subsetOf(context.currentParticipatingEndpoints.get)
    )(SeqProgParticipant(this))

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    assert(context.currentParticipatingEndpoints.isDefined)
    super.check(context) ++ allowedInner.toSeq ++
      Option.when(branches)(participantCheck(context)).flatten.toSeq
  }

  override def enterCheckContextCurrentParticipatingEndpoints(
      context: CheckContext[G]
  ): Option[Set[Endpoint[G]]] =
    if (branches) {
      // Assume SeqProg sets participatingEndpoints
      assert(context.currentParticipatingEndpoints.isDefined)

      if (hasUnpointed) {
        // Everyone that is participating keeps participating, as well as any endpoints explicitly mentioned
        context.appendCurrentParticipatingEndpoints(explicitEndpoints)
      } else {
        // We can refine the set of participants down to the set of endpoints actually present in the guard
        context.withCurrentParticipatingEndpoints(explicitEndpoints)
      }
    } else { context.currentParticipatingEndpoints }
}
