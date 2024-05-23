package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.RewriteVariable
import vct.col.ast._
import vct.col.rewrite.util.Extract
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, Substitute}

import scala.collection.mutable

case object ParBlockEncoder extends RewriterBuilder {
  override def key: String = "parBlock"

  override def desc: String =
    "Translate parallel blocks into methods and generate checks for them."

  private def LowEvalOrigin(v: IterVariable[_]): Origin =
    v.variable.o.where(prefix = "lo")
  private def HighEvalOrigin(v: IterVariable[_]): Origin =
    v.variable.o.where(prefix = "hi")

  case class ParBlockNotInjective(block: ParBlock[_], expr: Expr[_])
      extends Blame[ReceiverNotInjective] {
    override def blame(error: ReceiverNotInjective): Unit =
      block.blame.blame(ParPredicateNotInjective(block, expr))
  }

  case class ParStatementExhaleFailed(region: ParRegion[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      region.blame.blame(ParPreconditionFailed(error.failure, region))
  }

  case class ParSequenceProofFailed(region: ParRegion[_])
      extends Blame[FramedProofFailure] {
    override def blame(error: FramedProofFailure): Unit =
      error match {
        case preFailed @ FramedProofPreFailed(_, _) =>
          PanicBlame(
            "The precondition in the framed proof of a par sequence is `true`"
          ).blame(preFailed)
        case FramedProofPostFailed(failure, _) =>
          region.blame.blame(ParPreconditionFailed(failure, region))
      }
  }

  case class ParBlockProofFailed(block: ParBlock[_])
      extends Blame[FramedProofFailure] {
    override def blame(error: FramedProofFailure): Unit =
      error match {
        case FramedProofPreFailed(failure, _) =>
          // PB: this is a bit dubious, maybe instead inhale the par precondition and panic here?
          block.blame.blame(ParPreconditionFailed(failure, block))
        case FramedProofPostFailed(failure, _) =>
          block.blame.blame(ParBlockPostconditionFailed(failure, block))
      }
  }

  case class ParInvariantCannotBeExhaled(invariant: ParInvariant[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      invariant.blame
        .blame(ParInvariantNotEstablished(error.failure, invariant))
  }

  case class ParAtomicCannotBeExhaled(atomic: ParAtomic[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      atomic.blame.blame(ParInvariantNotMaintained(error.failure, atomic))
  }

  case class ParBarrierInvariantExhaleFailed(barrier: ParBarrier[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      barrier.blame.blame(ParBarrierInvariantBroken(error.failure, barrier))
  }

  case class ParBarrierExhaleFailed(barrier: ParBarrier[_])
      extends Blame[ExhaleFailed] {
    override def blame(error: ExhaleFailed): Unit =
      barrier.blame.blame(ParBarrierNotEstablished(error.failure, barrier))
  }

  case class ParBarrierProofFailed(barrier: ParBarrier[_])
      extends Blame[FramedProofFailure] {
    override def blame(error: FramedProofFailure): Unit =
      error match {
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

  val currentRanges: ScopedStack[Map[Variable[Pre], (Expr[Post], Expr[Post])]] =
    ScopedStack()

  // We need range values even in layered parallel blocks, since e.g. barriers need them.
  def range(v: Variable[Pre]): (Expr[Post], Expr[Post]) =
    currentRanges.find(_.contains(v)).get(v)

  def from(v: Variable[Pre]): Expr[Post] = range(v)._1

  def to(v: Variable[Pre]): Expr[Post] = range(v)._2

  def depVars[G](bindings: Set[Variable[G]], e: Expr[G]): Set[Variable[G]] = {
    val result: mutable.Set[Variable[G]] = mutable.Set()
    e.transSubnodes.foreach {
      case Local(ref) if bindings.contains(ref.decl) => result.addOne(ref.decl)
      case _ =>
    }
    result.toSet
  }

  def quantify(block: ParBlock[Pre], expr: Expr[Pre], nonEmpty: Boolean)(
      implicit o: Origin
  ): Expr[Post] = {
    val exprs = AstBuildHelpers.unfoldStar(expr)
    val vars = block.iters.map(_.variable).toSet

    val rewrittenExpr = exprs.map(e => {
      val quantVars =
        if (nonEmpty)
          depVars(vars, e)
        else
          vars
      val nonQuantVars = vars.diff(quantVars)

      val scale =
        (x: Expr[Post]) =>
          nonQuantVars.foldLeft(x)((body, iter) => {
            val scale = to(iter) - from(iter)
            Scale(scale, body)(PanicBlame(
              "Par block was checked to be non-empty"
            ))(body.o)
          })

      if (quantVars.isEmpty)
        scale(dispatch(e))
      else
        variables.scope {
          val range = quantVars.map(v =>
            from(v) <= Local[Post](succ(v)) && Local[Post](succ(v)) < to(v)
          ).reduceOption[Expr[Post]](And(_, _)).getOrElse(tt)

          e match {
            case Forall(bindings, Nil, body) =>
              Forall(
                variables.dispatch(bindings ++ quantVars),
                Nil,
                range ==> scale(dispatch(body)),
              )(body.o)
            case s @ Starall(bindings, Nil, body) =>
              Starall(
                variables.dispatch(bindings ++ quantVars),
                Nil,
                range ==> scale(dispatch(body)),
              )(s.blame)(body.o)
            case other =>
              Starall(
                variables.dispatch(quantVars),
                Nil,
                range ==> scale(dispatch(other)),
              )(ParBlockNotInjective(block, other))(other.o)
          }
        }
    })

    AstBuildHelpers.foldStar(rewrittenExpr)
  }

  def requires(region: ParRegion[Pre], nonEmpty: Boolean)(
      implicit o: Origin
  ): Expr[Post] =
    region match {
      case ParParallel(regions) =>
        AstBuildHelpers.foldStar(regions.map(requires(_, nonEmpty)))
      case ParSequential(regions) =>
        regions.headOption.map(requires(_, nonEmpty)).getOrElse(tt)
      case block: ParBlock[Pre] =>
        quantify(block, block.context_everywhere &* block.requires, nonEmpty)
    }

  def ensures(region: ParRegion[Pre], nonEmpty: Boolean)(
      implicit o: Origin
  ): Expr[Post] =
    region match {
      case ParParallel(regions) =>
        AstBuildHelpers.foldStar(regions.map(ensures(_, nonEmpty)))
      case ParSequential(regions) =>
        regions.lastOption.map(ensures(_, nonEmpty)).getOrElse(tt)
      case block: ParBlock[Pre] =>
        quantify(block, block.context_everywhere &* block.ensures, nonEmpty)
    }

  // A variable of this type cannot be altered in a parallel block
  def isConstType(t: Type[_]): Boolean =
    t match {
      case _: NumericType[_] => true
      case _: TBool[_] => true
      case _: TChar[_] => true
      case TSeq(e) => isConstType(e)
      case TSet(e) => isConstType(e)
      case TBag(e) => isConstType(e)
      case TMap(e, v) => isConstType(e) && isConstType(v)
      case TOption(e) => isConstType(e)
      case TTuple(es) => es.forall(isConstType)
      case TEither(l, r) => isConstType(l) && isConstType(r)
      case _ => false
    }

  def isConstant(e: Expr[Post]): Boolean =
    e match {
      case _: Constant[Post] => true
      case l: Local[_] if isConstType(l.t) => true
      case _: Constant[_] => true
      case op: BinExpr[Post] => isConstant(op.left) && isConstant(op.right)
      case _ => false
    }

  def ranges(
      region: ParRegion[Pre],
      rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])],
  ): Statement[Post] =
    region match {
      case ParParallel(regions) =>
        Block(regions.map(ranges(_, rangeValues)))(region.o)
      case ParSequential(regions) =>
        Block(regions.map(ranges(_, rangeValues)))(region.o)
      case block @ ParBlock(decl, iters, _, _, _, _) =>
        decl.drop()
        blockDecl(decl) = block
        Block(iters.foldLeft(Seq[Statement[Post]]()) { case (res, v) =>
          implicit val o: Origin = v.o
          val from = dispatch(v.from)
          val to = dispatch(v.to)
          val lo = variables
            .declare(new Variable[Post](TInt())(LowEvalOrigin(v)))
          val hi = variables
            .declare(new Variable[Post](TInt())(HighEvalOrigin(v)))
          val low =
            if (isConstant(from))
              from
            else
              lo.get
          val high =
            if (isConstant(to))
              to
            else
              hi.get
          rangeValues(v.variable) = (low, high)
          res ++ Seq(assignLocal(lo.get, from), assignLocal(hi.get, to))
        })(region.o)
    }

  def execute(region: ParRegion[Pre], nonEmpty: Boolean): Statement[Post] = {
    implicit val o: Origin = region.o
    Block(Seq(
      Exhale(requires(region, nonEmpty))(ParStatementExhaleFailed(region)),
      Inhale(ensures(region, nonEmpty)),
    ))
  }

  def check(region: ParRegion[Pre]): Statement[Post] =
    region match {
      case ParParallel(regions) => IndetBranch(regions.map(check))(region.o)
      case ParSequential(regions) =>
        IndetBranch(regions.map(check) ++ regions.zip(regions.tail).map {
          case (leftRegion, rightRegion) =>
            implicit val o: Origin = region.o
            FramedProof(
              tt,
              Inhale(ensures(leftRegion, false)),
              requires(rightRegion, false),
            )(ParSequenceProofFailed(rightRegion))
        })(region.o)
      case block @ ParBlock(
            decl,
            iters,
            context_everywhere,
            requires,
            ensures,
            content,
          ) =>
        implicit val o: Origin = region.o
        val (vars, (init, blockNonEmpty)) = variables.collect {
          val initsAndConds = iters.map { v =>
            dispatch(v.variable)
            val cond = from(v.variable) < to(v.variable)
            val init = assignLocal(
              Local[Post](succ(v.variable)),
              ChooseFresh(RangeSet(from(v.variable), to(v.variable)))(
                PanicBlame("under branch from < to")
              ),
            )
            (init, cond)
          }

          (Block(initsAndConds.map(_._1)), foldAnd(initsAndConds.map(_._2)))
        }

        val proof = Scope(
          vars,
          Block(Seq(
            init,
            FramedProof(
              pre = dispatch(context_everywhere) &* dispatch(requires),
              body = dispatch(content),
              post = dispatch(context_everywhere) &* dispatch(ensures),
            )(ParBlockProofFailed(block)),
          )),
        )

        Branch(Seq(blockNonEmpty -> proof))
    }

  def isParBlock(stat: ParRegion[Pre]): Boolean =
    stat match {
      case _: ParBlock[Pre] => true
      case _ => false
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case ParStatement(region) =>
        val isSingleBlock: Boolean = isParBlock(region)
        implicit val o: Origin = stat.o
        val rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])] =
          mutable.Map()
        val (vars, evalRanges) = variables.collect {
          ranges(region, rangeValues)
        }

        currentRanges.having(rangeValues.toMap) {
          var res: Statement[Post] = IndetBranch(Seq(
            execute(region, isSingleBlock),
            Block(Seq(check(region), Inhale(ff))),
          ))
          if (isSingleBlock) {
            val condition: Expr[Post] = foldAnd(rangeValues.values.map {
              case (low, hi) => low < hi
            })
            res = Branch(Seq((condition, res)))
          }
          Scope(vars, Block(Seq(evalRanges, res)))
        }

      case inv @ ParInvariant(decl, dependentInvariant, body) =>
        implicit val o: Origin = stat.o
        // PB: this usage is dubious: dependentInvariant can probably contain type variables?
        val (Seq(frozenInvariant), mappings) = Extract
          .extract(dependentInvariant)

        decl.drop()
        invDecl(decl) = frozenInvariant

        Scope(
          variables.collect { mappings.keys.foreach(dispatch) }._1,
          Block(Seq(
            Block(mappings.map { case (v, e) =>
              assignLocal[Post](Local(succ(v)), dispatch(e))
            }.toSeq),
            Exhale(dispatch(frozenInvariant))(ParInvariantCannotBeExhaled(inv)),
            dispatch(body),
            Inhale(dispatch(frozenInvariant)),
          )),
        )

      case atomic @ ParAtomic(invDecls, body) =>
        implicit val o: Origin = atomic.o
        Block(Seq(
          Block(invDecls.map { case Ref(decl) =>
            Inhale(dispatch(invDecl(decl)))
          }),
          dispatch(body),
          Block(invDecls.reverse.map { case Ref(decl) =>
            Exhale(dispatch(invDecl(decl)))(ParAtomicCannotBeExhaled(atomic))
          }),
        ))

      case barrier @ ParBarrier(
            Ref(decl),
            suspendedInvariants,
            requires,
            ensures,
            hint,
          ) =>
        implicit val o: Origin = barrier.o
        val block = blockDecl(decl)
        IndetBranch[Post](Seq(
          // Prove the barrier for all threads (and inhale false)
          Block(Seq(
            FramedProof(
              pre = tt,
              body = Block(Seq(
                Inhale(quantify(block, requires, false)),
                Block(suspendedInvariants.map { case Ref(decl) =>
                  Inhale(dispatch(invDecl(decl)))
                }),
                dispatch(hint),
                Block(suspendedInvariants.reverse.map { case Ref(decl) =>
                  Exhale(dispatch(invDecl(decl)))(
                    ParBarrierInvariantExhaleFailed(barrier)
                  )
                }),
              )),
              post = quantify(block, ensures, false),
            )(ParBarrierProofFailed(barrier)),
            Inhale(ff),
          )),
          // Enact the barrier contract for the current thread
          Block(Seq(
            Exhale(dispatch(requires))(ParBarrierExhaleFailed(barrier)),
            Inhale(dispatch(ensures)),
          )),
        ))

      case other => rewriteDefault(other)
    }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] =
    e match {
      case ScaleByParBlock(Ref(decl), res) if e.t == TResource[Pre]() =>
        implicit val o: Origin = e.o
        val block = blockDecl(decl)
        block.iters.foldLeft(dispatch(res)) { case (res, v) =>
          val scale = to(v.variable) - from(v.variable)
          Implies(
            scale > const(0),
            Scale(const[Post](1) /:/ scale, res)(PanicBlame("framed positive")),
          )
        }
      case ScaleByParBlock(Ref(_), res) => dispatch(res)
      case other => rewriteDefault(other)
    }
}
