package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteIterVariable, RewriteVariable}
import vct.col.ast._
import vct.col.newrewrite.ParBlockEncoder.ParBlockNotInjective
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, Substitute}

import scala.collection.mutable

case object NewParBlockEncoder extends RewriterBuilder {
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
}

case class NewParBlockEncoder[Pre <: Generation]() extends Rewriter[Pre] {
  import NewParBlockEncoder._

  val blockDecl: mutable.Map[ParBlockDecl[Pre], ParBlock[Pre]] = mutable.Map()

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

  def requires(region: ParRegion[Pre], includingInvariant: Boolean = false)(implicit o: Origin): Expr[Post] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(requires(_, includingInvariant)))
    case ParSequential(regions) => regions.headOption.map(requires(_, includingInvariant)).getOrElse(tt)
    case block: ParBlock[Pre] =>
      if(includingInvariant) dispatch(block.context_everywhere) &* quantify(block, block.requires) else quantify(block, block.requires)
  }

  def ensures(region: ParRegion[Pre], includingInvariant: Boolean = false)(implicit o: Origin): Expr[Post] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(ensures(_, includingInvariant)))
    case ParSequential(regions) => regions.lastOption.map(ensures(_, includingInvariant)).getOrElse(tt)
    case block: ParBlock[Pre] =>
      if(includingInvariant) dispatch(block.context_everywhere) &* quantify(block, block.ensures) else quantify(block, block.ensures)
  }

  def ranges(region: ParRegion[Pre], rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])]): Statement[Post] = region match {
    case ParParallel(regions) => Block(regions.map(ranges(_, rangeValues)))(region.o)
    case ParSequential(regions) => Block(regions.map(ranges(_, rangeValues)))(region.o)
    case block @ ParBlock(decl, iters, _, _, _, _) =>
      decl.drop()
      blockDecl(decl) = block
      Block(iters.map { v =>
        implicit val o: Origin = v.o
        val lo = new Variable(TInt())(LowEvalOrigin(v)).declareDefault(this)
        val hi = new Variable(TInt())(HighEvalOrigin(v)).declareDefault(this)
        rangeValues(v.variable) = (lo.get, hi.get)

        Block(Seq(
          assignLocal(lo.get, dispatch(v.from)),
          assignLocal(hi.get, dispatch(v.to)),
        ))
      })(region.o)
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
          pre = dispatch(requires),
          body = dispatch(content),
          post = dispatch(ensures),
        )(ParBlockProofFailed(block)),
      )))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = stat match {
    case ParStatement(region) =>
      implicit val o: Origin = stat.o

      val rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])] = mutable.Map()

      val (vars, evalRanges) = withCollectInScope(variableScopes) {
        ranges(region, rangeValues)
      }

      currentRanges.having(rangeValues.toMap) {
        Scope(vars, Block(Seq(
          evalRanges,
          IndetBranch(Seq(
            execute(region),
            Block(Seq(check(region), Inhale(ff)))
          )),
        )))
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case ScaleByParBlock(Ref(decl), res) =>
      implicit val o: Origin = e.o
      val block = blockDecl(decl)
      block.iters.foldLeft(dispatch(res)) {
        case (res, v) =>
          val scale = to(v.variable) - from(v.variable)
          Implies(scale > const(0), Scale(const[Post](1) /: scale, res)(PanicBlame("framed positive")))
      }
    case other => rewriteDefault(other)
  }
}
