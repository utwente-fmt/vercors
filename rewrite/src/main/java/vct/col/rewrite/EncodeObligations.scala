package vct.col.rewrite
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteApplicableContract
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

case object EncodeObligations extends RewriterBuilder {
  override def key: String = "encodeObligations"
  override def desc: String = "Translate obligations into predicates and for-perm leak checks"
}

case class EncodeObligations[Pre <: Generation]() extends Rewriter[Pre] {
  val obligationPredicate: SuccessionMap[Obligation[Pre], Predicate[Post]] = SuccessionMap()
  val allObligations: ScopedStack[Seq[Obligation[Pre]]] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Post] =
    allObligations.having(program.collect { case obl: Obligation[Pre] => obl }) {
      rewriteDefault(program)
    }

  def noLeaks(implicit o: Origin): Expr[Post] =
    foldStar(allObligations.top.map { obligation =>
      val bindings = obligation.args.map(arg => new Variable[Post](dispatch(arg.t))(arg.o))
      ForPerm(
        bindings = bindings,
        location = PredicateLocation(obligationPredicate.ref(obligation), bindings.map(_.get)),
        body = ff,
      )
    })

  def noLeaksOnExhale(implicit o: Origin): Expr[Post] = InhaleExhaleAssertion(tt, noLeaks)
  def noLeaksOnInhale(implicit o: Origin): Expr[Post] = InhaleExhaleAssertion(noLeaks, tt)

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case obligation: Obligation[Pre] =>
      obligation.drop()
      obligationPredicate(obligation) = new Predicate(
        args = variables.dispatch(obligation.args),
        body = None,
        threadLocal = false, inline = false
      )(obligation.o)

    case other => rewriteDefault(other)
  }

  override def dispatch(c: ApplicableContract[Pre]): ApplicableContract[Post] =
    c.rewrite(
      requires = dispatch(c.requires) &* tt
    )
}
