package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}
import vct.col.rewrite.util.WellDefinednessConditions
import vct.col.util.AstBuildHelpers._

// Checks loop/par invariants are satisfiable: inhale into an isolated FramedProof
// and refute false; a fired refute means unsatisfiable. WD conditions are inhaled
// one statement at a time so each is in scope when the next is checked.
case object CheckInvariantSatisfiability extends RewriterBuilderArg[Boolean] {
  override def key: String = "checkInvSat"
  override def desc: String =
    "Check that loop invariants and parallel invariants are not internally contradictory (i.e. unsatisfiable)."

  case class LoopInvariantUnsatisfiableBlame(li: LoopInvariant[_])
      extends Blame[RefuteFailed] {
    override def blame(error: RefuteFailed): Unit =
      li.blame.blame(LoopInvariantUnsatisfiable(li))
  }

  // ParInvariant's own blame only accepts ParInvariantNotEstablished, so this
  // anchors to the enclosing ContractApplicable instead.
  case class ParInvariantUnsatisfiableBlame(
      parInv: ParInvariant[_],
      anchor: ContractApplicable[_],
  ) extends Blame[RefuteFailed] {
    override def blame(error: RefuteFailed): Unit =
      anchor.blame.blame(ParInvariantUnsatisfiable(parInv))
  }

  // The empty heap here can spuriously flag well-definedness errors that don't
  // occur in the invariant's real position; suppress to avoid a duplicate report.
  case class IgnoreWellformednessInInvSat[T <: VerificationFailure]()
      extends Blame[T] {
    override def blame(error: T): Unit = ()
  }
}

case class CheckInvariantSatisfiability[Pre <: Generation](
    doCheck: Boolean = true
) extends Rewriter[Pre] {
  import CheckInvariantSatisfiability._

  val wellFormednessBlame: ScopedStack[Blame[VerificationFailure]] =
    ScopedStack()
  val currentApplicable: ScopedStack[ContractApplicable[Pre]] = ScopedStack()

  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] =
    wellFormednessBlame.topOption.getOrElse(blame)

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case ca: ContractApplicable[Pre] =>
        currentApplicable.having(ca) { super.dispatch(decl) }
      case _ => super.dispatch(decl)
    }

  // Builds the shared isolated-context check: an Extract/FramedProof that inhales
  // the enclosing method's non-heap preconditions, then `inv`, then refutes false.
  private def isolatedSatCheck(inv: Expr[Pre], blame: Blame[RefuteFailed])(
      implicit o: Origin
  ): Statement[Post] = {
    val nonHeapPreConds = currentApplicable.topOption.toSeq.flatMap { ca =>
      WellDefinednessConditions.splitPred(ca.contract.requires)
        .flatMap(WellDefinednessConditions.extractNonHeap)
    }

    Extract(
      FramedProof(
        tt,
        Block[Post](
          nonHeapPreConds.map(c =>
            Inhale(wellFormednessBlame.having(IgnoreWellformednessInInvSat()) {
              dispatch(c)
            })
          ) :+
            Inhale(wellFormednessBlame.having(IgnoreWellformednessInInvSat()) {
              dispatch(inv)
            }) :+ Refute(ff)(blame)
        ),
        tt,
      )(PanicBlame("FramedProof with trivial pre/post cannot fail")),
      None,
    )(PanicBlame("Extract without termination measure cannot fail"))
  }

  private def hasOld(inv: Expr[Pre]): Boolean =
    inv.exists { case _: Old[Pre] => true }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    if (!doCheck)
      stat.rewriteDefault()
    else
      stat match {
        // \old(e) isn't generally equivalent to e (e.g. "x == \old(x) + i" is sat,
        // "x == x + i" isn't for nonzero i), and the empty heap below has no old-state
        // snapshot anyway, so skip invariants containing \old(...) rather than risk a
        // false unsatisfiable.
        case loop @ Loop(_, _, _, LoopInvariant(inv, _), _) if hasOld(inv) =>
          loop.rewriteDefault()
        case parInv @ ParInvariant(_, inv, _) if hasOld(inv) =>
          parInv.rewriteDefault()

        case loop @ Loop(_, _, _, li @ LoopInvariant(inv, _), _) =>
          implicit val o: Origin = li.o.where(prefix = "checkInvSat")
          Block[Post](Seq[Statement[Post]](
            isolatedSatCheck(inv, LoopInvariantUnsatisfiableBlame(li)),
            loop.rewriteDefault(),
          ))

        case parInv @ ParInvariant(_, inv, _) =>
          currentApplicable.topOption match {
            // No enclosing method to anchor a report to: nothing to check.
            case None => parInv.rewriteDefault()
            case Some(anchor) =>
              implicit val o: Origin = parInv.o.where(prefix = "checkInvSat")
              Block[Post](Seq[Statement[Post]](
                isolatedSatCheck(
                  inv,
                  ParInvariantUnsatisfiableBlame(parInv, anchor),
                ),
                parInv.rewriteDefault(),
              ))
          }

        case other => other.rewriteDefault()
      }
}
