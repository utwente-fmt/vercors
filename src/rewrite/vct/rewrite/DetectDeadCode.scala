package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{
  Blame,
  ContractedFailure,
  DeadBranch,
  LoopInvariantFailure,
  LoopInvariantNotEstablished,
  LoopInvariantNotMaintained,
  Origin,
  RefuteFailed,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.ff

case object DetectDeadCode extends RewriterBuilder {
  override def key: String = "detectDeadCode"
  override def desc: String =
    "Check that branches, loop bodies, switch cases, and code after state-narrowing statements are reachable under the method preconditions and assumptions."

  class DeadCodeBlame(
      branchNode: Node[_],
      branchKind: String,
      methodBlame: Blame[ContractedFailure],
      ancestorFired: () => Boolean,
  ) extends Blame[RefuteFailed] {
    private var firedOrSuppressed: Boolean = false

    def didFire: Boolean = firedOrSuppressed

    override def blame(error: RefuteFailed): Unit = {
      firedOrSuppressed = true
      if (!ancestorFired())
        methodBlame.blame(DeadBranch(branchNode, branchKind))
    }
  }
}

case class DetectDeadCode[Pre <: Generation]() extends Rewriter[Pre] {
  import DetectDeadCode.DeadCodeBlame

  val methodBlame: ScopedStack[Blame[ContractedFailure]] = ScopedStack()
  val currentBlame: ScopedStack[DeadCodeBlame] = ScopedStack()
  val afterAssertFalse: ScopedStack[Unit] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case ca: ContractApplicable[Pre] =>
        methodBlame.having(ca.blame) { super.dispatch(ca) }
      case _ => super.dispatch(decl)
    }

  private def condText(expr: Expr[Pre]): String = {
    val text = expr.o.inlineContextText
    if (text == "[unknown context]") "<expression>" else text
  }

  private def makeCheck(
      node: Node[Pre],
      branchKind: String,
      extraSuppressor: () => Boolean = () => false,
  ): (DeadCodeBlame, Statement[Post]) = {
    implicit val o: Origin = node.o.where(prefix = "deadCode")
    val ancestorFiredFn: () => Boolean = {
      val base = currentBlame.topOption.map(b => () => b.didFire).getOrElse(() => false)
      () => base() || extraSuppressor()
    }
    val blame = new DeadCodeBlame(node, branchKind, methodBlame.top, ancestorFiredFn)
    (blame, Refute(ff)(blame))
  }

  private def instrumentBody(
      node: Node[Pre],
      label: String,
      body: Statement[Pre],
      originNode: Node[Pre] = null,
      extraSuppressor: () => Boolean = () => false,
  )(implicit o: Origin): Statement[Post] = {
    val origin = if (originNode != null) originNode else node
    val (blame, check) = makeCheck(origin, label, extraSuppressor)
    currentBlame.having(blame) { Block(Seq(check, dispatch(body))) }
  }

  private def appendCheck(node: Statement[Pre], label: String): Statement[Post] = {
    implicit val o: Origin = node.o
    val (blame, check) = makeCheck(node, label)
    Block(Seq(node.rewriteDefault(), currentBlame.having(blame) { check }))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    if (methodBlame.topOption.isEmpty || afterAssertFalse.nonEmpty) stat.rewriteDefault()
    else stat match {
      case branch @ Branch(Seq((cond, thenBody), (BooleanValue(true), elseBody))) =>
        implicit val o: Origin = branch.o
        Branch(Seq(
          (dispatch(cond),
            instrumentBody(branch, s"then-branch (condition: `${condText(cond)}`)", thenBody, originNode = thenBody)),
          (dispatch(BooleanValue(true)),
            instrumentBody(branch, s"else-branch (negation of condition: `${condText(cond)}`)", elseBody, originNode = elseBody)),
        ))

      // if (cond) { ... }  (no else)
      case branch @ Branch(Seq((cond, thenBody))) =>
        implicit val o: Origin = branch.o
        Branch(Seq(
          (dispatch(cond),
            instrumentBody(branch, s"then-branch (condition: `${condText(cond)}`)", thenBody)),
        ))

      case loop @ Loop(init, cond, update, contract, body) =>
        implicit val o: Origin = loop.o
        var invNotEstablishedFired = false
        var invNotMaintainedFired = false
        val bodySuppress: () => Boolean = () => invNotEstablishedFired
        val postLoopSuppress: () => Boolean = () => invNotEstablishedFired || invNotMaintainedFired

        val (invInfo, dispatchedContract) = contract match {
          case li: LoopInvariant[Pre] =>
            val wrappedBlame = new Blame[LoopInvariantFailure] {
              override def blame(error: LoopInvariantFailure): Unit = {
                error match {
                  case _: LoopInvariantNotEstablished => invNotEstablishedFired = true
                  case _: LoopInvariantNotMaintained  => invNotMaintainedFired = true
                  case _                              =>
                }
                li.blame.blame(error)
              }
            }
            (
              s", invariant: `${condText(li.invariant)}`",
              li.rewrite(blame = wrappedBlame),
            )
          case other => ("", dispatch(other))
        }

        val instrumentedLoop = Loop(
          dispatch(init),
          dispatch(cond),
          dispatch(update),
          dispatchedContract,
          instrumentBody(
            loop,
            s"loop body (condition: `${condText(cond)}`$invInfo)",
            body,
            extraSuppressor = bodySuppress,
          ),
        )
        val (afterBlame, afterCheck) =
          makeCheck(
            loop,
            s"code after loop (condition: `${condText(cond)}`$invInfo)",
            extraSuppressor = postLoopSuppress,
          )
        Block(Seq(instrumentedLoop, currentBlame.having(afterBlame) { afterCheck }))

      case block @ Block(stmts) =>
        implicit val o: Origin = block.o
        val cutoffIdx = stmts.indexWhere {
          case Assert(BooleanValue(false)) => true
          case _ => false
        }
        if (cutoffIdx < 0) {
          Block(stmts.map(dispatch))
        } else {
          val (before, rest) = stmts.splitAt(cutoffIdx + 1)
          Block(before.map(dispatch) ++ afterAssertFalse.having(()) { rest.map(dispatch) })
        }

      case l @ Lock(obj)              => appendCheck(l, s"code after locking `${condText(obj)}`")
      case u @ Unfold(res)            => appendCheck(u, s"code after unfolding `${condText(res)}`")
      case a @ Assume(BooleanValue(false)) =>
        implicit val o: Origin = a.o
        methodBlame.top.blame(DeadBranch(a, "assume false"))
        Inhale(ff)
      case i @ Inhale(BooleanValue(false)) =>
        methodBlame.top.blame(DeadBranch(i, "inhale false"))
        i.rewriteDefault()
      case a @ Assume(assn)           => appendCheck(a, s"code after assume `${condText(assn)}`")
      case i @ Inhale(res)            => appendCheck(i, s"code after inhale `${condText(res)}`")

      case c: SwitchCase[Pre] =>
        implicit val o: Origin = c.o
        val label = c match {
          case Case(pattern) => s"switch case (pattern: `${condText(pattern)}`)"
          case DefaultCase() => "switch default case"
        }
        val (caseBlame, caseCheck) = makeCheck(c, label)
        Block(Seq(c.rewriteDefault(), currentBlame.having(caseBlame) { caseCheck }))

      case atomic @ ParAtomic(_, content) =>
        implicit val o: Origin = atomic.o
        atomic.rewrite(content =
          instrumentBody(atomic, "atomic block body", content))

      case other => other.rewriteDefault()
    }

  override def dispatch(node: ParRegion[Pre]): ParRegion[Post] =
    if (methodBlame.topOption.isEmpty) node.rewriteDefault()
    else node match {
      case block @ ParBlock(_, _, _, _, _, content) =>
        implicit val o: Origin = block.o
        block.rewrite(content =
          instrumentBody(block, "parallel block body", content))
      case other => other.rewriteDefault()
    }
}