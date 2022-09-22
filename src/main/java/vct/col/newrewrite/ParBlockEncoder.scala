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

  def depVars[G](bindings: Set[Variable[G]], e: Expr[G]): Set[Variable[G]] = {
    val result: mutable.Set[Variable[G]] = mutable.Set()
    e.transSubnodes.foreach {
      case Local(ref) if bindings.contains(ref.decl) => result.addOne(ref.decl)
      case _ => }
    result.toSet
  }

  def quantify(block: ParBlock[Pre], expr: Expr[Pre], nonEmpty: Boolean)(implicit o: Origin): Expr[Post] = {
    val exprs = AstBuildHelpers.unfoldStar(expr)
    val vars = block.iters.map(_.variable).toSet

    val rewrittenExpr = exprs.map(
      e => {
        val quantVars = if (nonEmpty) depVars(vars, e) else vars
        val nonQuantVars = vars.diff(quantVars)
        val newQuantVars = quantVars.map(v => v -> new Variable[Pre](v.t)(v.o)).toMap
        var body = dispatch(Substitute(
            newQuantVars.map { case (l, r) => Local[Pre](l.ref) -> Local[Pre](r.ref) }.toMap[Expr[Pre], Expr[Pre]]
          ).dispatch(e))
        body = if (e.t == TResource[Pre]()) {
          // Scale the body if it contains permissions
          nonQuantVars.foldLeft(body)((body, iter) => {
            val scale = to(iter) - from(iter)
            Scale(scale, body)(PanicBlame("Par block was checked to be non-empty"))
          })
        } else {
          body
        }
        // Result, quantify over all the relevant variables
        quantVars.foldLeft(body)((body, iter) => {
          val v: Variable[Pre] = newQuantVars(iter)
          Starall[Post](
            Seq(variables.dispatch(v)),
            Nil,
            (from(iter) <= Local[Post](succ(v)) && Local[Post](succ(v)) < to(iter)) ==> body
          )(ParBlockNotInjective(block, e))
        })
      }
    )
    AstBuildHelpers.foldStar(rewrittenExpr)
  }

  def requires(region: ParRegion[Pre], nonEmpty: Boolean)(implicit o: Origin): Expr[Post] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(requires(_, nonEmpty)))
    case ParSequential(regions) => regions.headOption.map(requires(_, nonEmpty)).getOrElse(tt)
    case block: ParBlock[Pre] =>
      quantify(block, block.context_everywhere &* block.requires, nonEmpty)
  }

  def ensures(region: ParRegion[Pre], nonEmpty: Boolean)(implicit o: Origin): Expr[Post] = region match {
    case ParParallel(regions) => AstBuildHelpers.foldStar(regions.map(ensures(_, nonEmpty)))
    case ParSequential(regions) => regions.lastOption.map(ensures(_, nonEmpty)).getOrElse(tt)
    case block: ParBlock[Pre] =>
      quantify(block, block.context_everywhere &* block.ensures, nonEmpty)
  }

  def constantExpression[A](e: Expr[A], nonConstVars: Set[Variable[A]]): Boolean = e match {
    case _: Constant[_, _] => true
    case op: BinExpr[_] => constantExpression(op.left, nonConstVars) && constantExpression(op.right, nonConstVars)
    case Local(v) => !nonConstVars.contains(v.decl)
    case _ => false
  }

  def applyFunc[A,B](f: A => Option[Set[B]], xs: Iterable[A]): Option[Set[B]] =
    xs.foldLeft(Some(Set()): Option[Set[B]]){case (res, x) => res.flatMap(r => f(x).map(s => r ++ s))}

  def combine[A](xs: Option[Set[A]], ys: Option[Set[A]]): Option[Set[A]] =
    xs.flatMap(xs => ys.map(ys => xs ++ ys))

  def scanIter(xs: Iterable[Statement[Pre]]): Option[Set[Variable[Pre]]] =
    applyFunc(scanForAssign, xs)

  def scanIterE(xs: Iterable[Expr[Pre]]): Option[Set[Variable[Pre]]] =
    applyFunc(scanForAssignE, xs)

  /* Scans statements and returns a set of variables, which will change value. Meaning other variables, did not change value.
  * The analysis is conservative, if we are not sure, we return None, meaning we are not sure about any variable if it
  * is potentially constant
   */
  def scanForAssign(s: Statement[Pre]): Option[Set[Variable[Pre]]] = s match {
    case Block(statements) => scanIter(statements)
    case Scope(_, statement) => scanForAssign(statement)
    case Branch(branches) =>
      for {
        set1 <- scanIter(branches.map(_._2))
        set2 <- scanIterE(branches.map(_._1))
      } yield set1 ++ set2
    case IndetBranch(statements) => scanIter(statements)
    case Switch(expr, body) => combine(scanForAssignE(expr), scanForAssign(body))
    case Loop(init, cond, update, _, body) =>
      scanForAssign(init).flatMap(r1 => scanForAssignE(cond).flatMap(r2 => scanForAssign(update)
        .flatMap(r3 => scanForAssign(body).map(r4 => r1 ++ r2 ++ r3 ++ r4))))
    case Assign(target, value) => combine(scanAssignTarget(target), scanForAssignE(value))
    case ParStatement(par) => scanForAssignP(par)
    case ParBarrier(_, _, _, _, content) => scanForAssign(content)
    case Eval(e) => scanForAssignE(e)
    case _ => None
  }

  def scanForAssignP(par: ParRegion[Pre]): Option[Set[Variable[Pre]]] = par match {
    case ParParallel(pars) => applyFunc(scanForAssignP, pars)
    case ParSequential(pars) => applyFunc(scanForAssignP, pars)
    case ParBlock(_, _, _, _, _, content) => scanForAssign(content)
  }

  def scanForAssignE(e: Expr[Pre]): Option[Set[Variable[Pre]]] = e match {
    case _ : Constant[Pre, _] => Some(Set())
    case Local(_) => Some(Set())
    case op : BinExpr[Pre] => combine(scanForAssignE(op.left), scanForAssignE(op.right))
    case ProcedureInvocation(_, args, outArgs, _, givenMap, yields) =>
        // Primitive types cannot be changed due to a function call, so we filter them
        // Other arguments can possibly be changed, so we collect them
      applyFunc(scanAssignTarget, args.filterNot(e => isPrimitive(e.t)))
        .flatMap(r1 => scanIterE(givenMap.map(_._2))
          // TODO This is how yield works right? The first variable of a yield tupple gets assigned to?
          .map(r2 => r1 ++ r2 ++ outArgs.map(a => a.decl) ++ yields.map(y => y._1.decl)))
    case FunctionInvocation(_, args, _, givenMap, yields) =>
      applyFunc(scanAssignTarget, args.filterNot(e => isPrimitive(e.t)))
        .flatMap(r1 => scanIterE(givenMap.map(_._2))
          .map(r2 => r1 ++ r2 ++ yields.map(y => y._1.decl)))
    case PreAssignExpression(target, value) => combine(scanAssignTarget(target), scanForAssignE(value))
    case PointerSubscript(pointer, index) => combine(scanForAssignE(pointer), scanForAssignE(index))
    case ArraySubscript(pointer, index) => combine(scanForAssignE(pointer), scanForAssignE(index))
    case SeqSubscript(seq, index) => combine(scanForAssignE(seq), scanForAssignE(index))
    case _ => None
  }

  def scanAssignTarget(e: Expr[Pre]): Option[Set[Variable[Pre]]] = e match {
    case Local(v) => Some(Set(v.decl))
    case ArraySubscript(arr, index) => combine(scanAssignTarget(arr),  scanForAssignE(index))
    case PointerSubscript(pointer, index) => combine(scanAssignTarget(pointer),  scanForAssignE(index))
    case SeqSubscript(seq, index) => combine(scanAssignTarget(seq),  scanForAssignE(index))
    case _ => None
  }

  def isPrimitive(t: Type[_]): Boolean = t match {
    case _: PrimitiveType[_] => true
    case _ => false
  }

  case class NonConstantVariables(vars: Set[Variable[Pre]])

  def ranges(region: ParRegion[Pre], rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])]): Statement[Post] = region match {
    case ParParallel(regions) => Block(regions.map(ranges(_, rangeValues)))(region.o)
    case ParSequential(regions) => Block(regions.map(ranges(_, rangeValues)))(region.o)
    case block @ ParBlock(decl, iters, _, _, _, _) =>
      decl.drop()
      blockDecl(decl) = block
      Block(iters.foldLeft(Seq[Statement[Post]]()) { case(res,v) =>
        implicit val o: Origin = v.o
        val from = dispatch(v.from)
        val to = dispatch(v.to)
        val nonConstVars = scanForAssign(block.content)
        if(nonConstVars.nonEmpty && constantExpression(v.from, nonConstVars.get) && constantExpression (v.to, nonConstVars.get)){
          rangeValues(v.variable) = (from, to)
        res
        } else {
          val lo = variables.declare(new Variable[Post](TInt())(LowEvalOrigin(v)))
          val hi = variables.declare(new Variable[Post](TInt())(HighEvalOrigin(v)))
          rangeValues(v.variable) = (lo.get, hi.get)
          res ++ Seq(
            assignLocal(lo.get, dispatch(v.from)),
            assignLocal(hi.get, dispatch(v.to)),
          )
        }
      })(region.o)
  }

  def execute(region: ParRegion[Pre], nonEmpty: Boolean): Statement[Post] = {
    implicit val o: Origin = region.o
    Block(Seq(
      Exhale(requires(region, nonEmpty))(ParStatementExhaleFailed(region)),
      Inhale(ensures(region, nonEmpty)),
    ))
  }

  def check(region: ParRegion[Pre]): Statement[Post] = region match {
    case ParParallel(regions) =>
      IndetBranch(regions.map(check))(region.o)
    case ParSequential(regions) =>
      IndetBranch(regions.map(check) ++ regions.zip(regions.tail).map {
        case (leftRegion, rightRegion) =>
          implicit val o: Origin = region.o
          FramedProof(tt, Inhale(ensures(leftRegion, false)), requires(rightRegion, false))(ParSequenceProofFailed(rightRegion))
      })(region.o)
    case block @ ParBlock(decl, iters, context_everywhere, requires, ensures, content) =>
      implicit val o: Origin = region.o
      val (vars, init) = variables.collect {
        Block(iters.map { v =>
          dispatch(v.variable)
          assignLocal(Local[Post](succ(v.variable)), IndeterminateInteger(from(v.variable), to(v.variable)))
        }) }
      Scope(vars, Block(Seq(
        init,
        FramedProof(
          pre = dispatch(context_everywhere) &* dispatch(requires),
          body = dispatch(content),
          post = dispatch(context_everywhere) &* dispatch(ensures),
        )(ParBlockProofFailed(block)),
      )))
  }

  def isParBlock(stat: ParRegion[Pre]): Boolean = stat match {
    case _: ParBlock[Pre] => true
    case _ => false
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case ParStatement(region) =>
      val isSingleBlock: Boolean = isParBlock(region)
      implicit val o: Origin = stat.o
      val rangeValues: mutable.Map[Variable[Pre], (Expr[Post], Expr[Post])] = mutable.Map()
      val (vars, evalRanges) = variables.collect {
        ranges(region, rangeValues)
      }

      currentRanges.having(rangeValues.toMap) {
        var res: Statement[Post] =
          IndetBranch(Seq(
            execute(region, isSingleBlock),
            Block(Seq(check(region), Inhale(ff)))
          ))
        if(isSingleBlock){
          val condition: Expr[Post] = foldAnd(rangeValues.values.map{case (low, hi) => low < hi})
          res = Branch(Seq((condition, res)))
        }
        Scope(vars,
          Block(Seq(evalRanges,res)))
      }

    case inv @ ParInvariant(decl, dependentInvariant, body) =>
      implicit val o: Origin = stat.o
      val (Seq(frozenInvariant), mappings) = Extract.extract(dependentInvariant)

      decl.drop()
      invDecl(decl) = frozenInvariant

      Scope(variables.collect { mappings.keys.foreach(dispatch) }._1, Block(Seq(
        Block(mappings.map { case (v, e) => assignLocal[Post](Local(succ(v)), dispatch(e)) }.toSeq),
        Exhale(dispatch(frozenInvariant))(ParInvariantCannotBeExhaled(inv)),
        dispatch(body),
        Inhale(dispatch(frozenInvariant)),
      )))

    case atomic @ ParAtomic(invDecls, body) =>
      implicit val o: Origin = atomic.o
      Block(Seq(
        Block(invDecls.map { case Ref(decl) => Inhale(dispatch(invDecl(decl))) }),
        dispatch(body),
        Block(invDecls.reverse.map { case Ref(decl) => Exhale(dispatch(invDecl(decl)))(ParAtomicCannotBeExhaled(atomic)) }),
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
              Inhale(quantify(block, requires, false)),
              Block(suspendedInvariants.map { case Ref(decl) => Inhale(dispatch(invDecl(decl))) }),
              dispatch(hint),
              Block(suspendedInvariants.reverse.map {
                case Ref(decl) => Exhale(
                  dispatch(invDecl(decl))
                )(ParBarrierInvariantExhaleFailed(barrier))
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
        ))
      ))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case ScaleByParBlock(Ref(decl), res) if e.t == TResource[Pre]() =>
      implicit val o: Origin = e.o
      val block = blockDecl(decl)
      block.iters.foldLeft(dispatch(res)) {
        case (res, v) =>
          val scale = to(v.variable) - from(v.variable)
          Implies(scale > const(0), Scale(const[Post](1) /:/ scale, res)(PanicBlame("framed positive")))
      }
    case ScaleByParBlock(Ref(_), res) => dispatch(res)
    case other => rewriteDefault(other)
  }
}
