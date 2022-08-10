package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteVariable
import vct.col.ast._
import vct.col.newrewrite.util.Extract
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, Substitute}

import scala.collection.mutable

case object ParBlockEncoder extends RewriterBuilder {
  override def key: String = "parBlock"
  override def desc: String = "Translate parallel blocks into methods and generate checks for them."

  case class LowEvalOrigin(v: IterVariable[_]) extends Origin {
    override def preferredName: String = "lo_" + v.variable.o.preferredName
    override def context: String = v.variable.o.context
    override def inlineContext: String = v.variable.o.inlineContext
    override def shortPosition: String = v.variable.o.shortPosition
  }

  case class HighEvalOrigin(v: IterVariable[_]) extends Origin {
    override def preferredName: String = v.variable.o.preferredName + "_hi"
    override def context: String = v.variable.o.context
    override def inlineContext: String = v.variable.o.inlineContext
    override def shortPosition: String = v.variable.o.shortPosition
  }

  case class ParBlockNotInjective(block: ParBlock[_], expr: Expr[_]) extends Blame[ReceiverNotInjective] {
    override def blame(error: ReceiverNotInjective): Unit =
      block.blame.blame(ParPredicateNotInjective(block, expr))
  }

  case class ParStatementExhaleFailed(region: ParRegion[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      region.blame.blame(ParPreconditionFailed(error.failure, region))
  }

  case class ParSequenceProofFailed(region: ParRegion[_]) extends Blame[FramedProofFailure] {
    override def blame(error: FramedProofFailure): Unit = error match {
      case preFailed @ FramedProofPreFailed(_, _) =>
        PanicBlame("The precondition in the framed proof of a par sequence is `true`").blame(preFailed)
      case FramedProofPostFailed(failure, _) =>
        region.blame.blame(ParPreconditionFailed(failure, region))
    }
  }

  case class ParBlockProofFailed(block: ParBlock[_]) extends Blame[FramedProofFailure] {
    override def blame(error: FramedProofFailure): Unit = error match {
      case FramedProofPreFailed(failure, _) =>
        // PB: this is a bit dubious, maybe instead inhale the par precondition and panic here?
        block.blame.blame(ParPreconditionFailed(failure, block))
      case FramedProofPostFailed(failure, _) =>
        block.blame.blame(ParBlockPostconditionFailed(failure, block))
    }
  }

  case class ParInvariantCannotBeExhaled(invariant: ParInvariant[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      invariant.blame.blame(ParInvariantNotEstablished(error.failure, invariant))
  }

  case class ParAtomicCannotBeExhaled(atomic: ParAtomic[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      atomic.blame.blame(ParInvariantNotMaintained(error.failure, atomic))
  }

  case class ParBarrierInvariantExhaleFailed(barrier: ParBarrier[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      barrier.blame.blame(ParBarrierInvariantBroken(error.failure, barrier))
  }

  case class ParBarrierExhaleFailed(barrier: ParBarrier[_]) extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      barrier.blame.blame(ParBarrierNotEstablished(error.failure, barrier))
  }

  case class ParBarrierProofFailed(barrier: ParBarrier[_]) extends Blame[FramedProofFailure] {
    override def blame(error: FramedProofFailure): Unit = error match {
      case FramedProofPreFailed(_, _) =>
        PanicBlame("Barrier consistency proof requires true").blame(error)
      case FramedProofPostFailed(failure, _) =>
        barrier.blame.blame(ParBarrierInconsistent(failure, barrier))
    }
  }
}

case class ParBlockEncoder[Pre <: Generation]() extends Rewriter[Pre] {
  import ParBlockEncoder._

  val blockDecl: mutable.Map[ParBlockDecl[Pre], ParBlock[Pre]] = mutable.Map()
  val invDecl: mutable.Map[ParInvariantDecl[Pre], Expr[Pre]] = mutable.Map()

  val currentRanges: ScopedStack[Map[Variable[Pre], (Expr[Post], Expr[Post])]] = ScopedStack()

  // We need range values even in layered parallel blocks, since e.g. barriers need them.
  def range(v: Variable[Pre]): (Expr[Post], Expr[Post]) =
    currentRanges.find(_.contains(v)).get(v)

  def from(v: Variable[Pre]): Expr[Post] = range(v)._1
  def to(v: Variable[Pre]): Expr[Post] = range(v)._2

  def quantify(block: ParBlock[Pre], expr: Expr[Pre])(implicit o: Origin): Expr[Post] = {
    val quantVars = block.iters.map(_.variable).map(v => v -> new Variable[Pre](v.t)(v.o)).toMap
    val body = Substitute(quantVars.map { case (l, r) => Local[Pre](l.ref) -> Local[Pre](r.ref) }.toMap[Expr[Pre], Expr[Pre]]).dispatch(expr)
    block.iters.foldLeft(dispatch(body))((body, iter) => {
      val v = quantVars(iter.variable)
      Starall[Post](
        collectInScope(variableScopes) { dispatch(v) },
        Nil,
        (from(iter.variable) <= Local[Post](succ(v)) && Local[Post](succ(v)) < to(iter.variable)) ==> body
      )(ParBlockNotInjective(block, expr))
    })
  }

  def requires(region: ParRegion[Pre])(implicit o: Origin): Expr[Post] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(requires))
    case ParSequential(regions) => regions.headOption.map(requires).getOrElse(tt)
    case block: ParBlock[Pre] =>
      quantify(block, block.context_everywhere &* block.requires)
  }

  def ensures(region: ParRegion[Pre])(implicit o: Origin): Expr[Post] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(ensures))
    case ParSequential(regions) => regions.lastOption.map(ensures).getOrElse(tt)
    case block: ParBlock[Pre] =>
      quantify(block, block.context_everywhere &* block.ensures)
  }

  def ranges(region: ParRegion[Pre], rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])]): Unit = region match {
    case ParParallel(regions) => regions.foreach(ranges(_, rangeValues))
    case ParSequential(regions) => regions.foreach(ranges(_, rangeValues))
    case block @ ParBlock(decl, iters, _, _, _, _) =>
      decl.drop()
      blockDecl(decl) = block
      iters.foreach { v =>
        rangeValues(v.variable) = (dispatch(v.from), dispatch(v.to))
      }
  }

  def execute(region: ParRegion[Pre]): Statement[Post] = {
    implicit val o: Origin = region.o
    Block(Seq(
      Exhale(requires(region))(ParStatementExhaleFailed(region)),
      Inhale(ensures(region)),
    ))
  }

  def check(region: ParRegion[Pre]): Statement[Post] = region match {
    case ParParallel(regions) =>
      IndetBranch(regions.map(check))(region.o)
    case ParSequential(regions) =>
      IndetBranch(regions.map(check) ++ regions.zip(regions.tail).map {
        case (leftRegion, rightRegion) =>
          implicit val o: Origin = region.o
          FramedProof(tt, Inhale(ensures(leftRegion)), requires(rightRegion))(ParSequenceProofFailed(rightRegion))
      })(region.o)
    case block @ ParBlock(decl, iters, context_everywhere, requires, ensures, content) =>
      implicit val o: Origin = region.o
      val (vars, init) = withCollectInScope(variableScopes) {
        Block(iters.map { v =>
          val newVar = v.variable.rewrite().succeedDefault(v.variable)
          assignLocal(newVar.get, IndeterminateInteger(from(v.variable), to(v.variable)))
        })
      }

      Scope(vars, Block(Seq(
        init,
        FramedProof(
          pre = dispatch(context_everywhere) &* dispatch(requires),
          body = dispatch(content),
          post = dispatch(context_everywhere) &* dispatch(ensures),
        )(ParBlockProofFailed(block)),
      )))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = stat match {
    case ParStatement(region) =>
      implicit val o: Origin = stat.o

      val rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])] = mutable.Map()

      ranges(region, rangeValues)

      currentRanges.having(rangeValues.toMap) {
        Block(Seq(
          IndetBranch(Seq(
            execute(region),
            Block(Seq(check(region), Inhale(ff)))
          )),
        ))
      }

    case inv @ ParInvariant(decl, dependentInvariant, body) =>
      implicit val o: Origin = stat.o
      val (Seq(frozenInvariant), mappings) = Extract.extract(dependentInvariant)

      decl.drop()
      invDecl(decl) = frozenInvariant

      Scope(collectInScope(variableScopes) { mappings.keys.foreach(dispatch) }, Block(Seq(
        Block(mappings.map { case (v, e) => assignLocal[Post](Local(succ(v)), dispatch(e)) }.toSeq),
        Exhale(freshSuccessionScope { dispatch(frozenInvariant) })(ParInvariantCannotBeExhaled(inv)),
        dispatch(body),
        Inhale(freshSuccessionScope { dispatch(frozenInvariant) }),
      )))

    case atomic @ ParAtomic(invDecls, body) =>
      implicit val o: Origin = atomic.o
      Block(Seq(
        Block(invDecls.map { case Ref(decl) => freshSuccessionScope { Inhale(dispatch(invDecl(decl))) } }),
        dispatch(body),
        Block(invDecls.reverse.map { case Ref(decl) => freshSuccessionScope { Exhale(dispatch(invDecl(decl)))(ParAtomicCannotBeExhaled(atomic)) } }),
      ))

    case barrier @ ParBarrier(Ref(decl), suspendedInvariants, requires, ensures, hint) =>
      implicit val o: Origin = barrier.o
      val block = blockDecl(decl)
      IndetBranch[Post](Seq(
        // Prove the barrier for all threads (and inhale false)
        Block(Seq(
          FramedProof(
            pre = tt,
            body = Block(Seq(
              Inhale(quantify(block, requires)),
              Block(suspendedInvariants.map { case Ref(decl) => Inhale(freshSuccessionScope { dispatch(invDecl(decl)) }) }),
              dispatch(hint),
              Block(suspendedInvariants.reverse.map {
                case Ref(decl) => Exhale(freshSuccessionScope {
                  dispatch(invDecl(decl))
                })(ParBarrierInvariantExhaleFailed(barrier))
              }),
            )),
            post = quantify(block, ensures),
          )(ParBarrierProofFailed(barrier)),
          Inhale(ff),
        )),
        // Enact the barrier contract for the current thread
        Block(Seq(
          Exhale(dispatch(requires))(ParBarrierExhaleFailed(barrier)),
          Inhale(dispatch(ensures)),
        ))
      ))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case ScaleByParBlock(Ref(decl), res) =>
      implicit val o: Origin = e.o
      val block = blockDecl(decl)
      block.iters.foldLeft(dispatch(res)) {
        case (res, v) =>
          val scale = to(v.variable) - from(v.variable)
          Implies(scale > const(0), Scale(const[Post](1) /:/ scale, res)(PanicBlame("framed positive")))
      }
    case other => rewriteDefault(other)
  }
}
