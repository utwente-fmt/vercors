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
  NamePrefix,
  Origin,
  RefuteFailed,
}
import vct.col.rewrite.{
  Generation,
  Rewriter,
  RewriterBuilder,
  RewriterBuilderArg,
}
import vct.col.util.AstBuildHelpers.ff

// Instruments branches, loop bodies, switch cases, and code after state-narrowing
// statements with Refute(false). If the prover state is unreachable at that point,
// refute fires and deadBranch is reported. Ancestor suppression prevents cascading errors.
case object DetectDeadCode extends RewriterBuilderArg[Boolean] {
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
      // only report if no enclosing dead region already fired
      if (!ancestorFired())
        methodBlame.blame(DeadBranch(branchNode, branchKind))
    }
  }
}

case class DetectDeadCode[Pre <: Generation](doCheck: Boolean = true)
    extends Rewriter[Pre] {
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
    if (text == "[unknown context]")
      "<expression>"
    else
      text
  }

  private def makeCheck(
      node: Node[Pre],
      branchKind: String,
      extraSuppressor: () => Boolean = () => false,
  ): (DeadCodeBlame, Statement[Post]) = {
    implicit val o: Origin = node.o.where(prefix = "deadCode")
    val ancestorFiredFn: () => Boolean = {
      val base = currentBlame.topOption.map(b => () => b.didFire)
        .getOrElse(() => false)
      () => base() || extraSuppressor()
    }
    val blame =
      new DeadCodeBlame(node, branchKind, methodBlame.top, ancestorFiredFn)
    (blame, Refute(ff)(blame))
  }

  // prepends Refute(false) before the body
  private def instrumentBody(
      node: Node[Pre],
      label: String,
      body: Statement[Pre],
      originNode: Node[Pre] = null,
      extraSuppressor: () => Boolean = () => false,
  )(implicit o: Origin): Statement[Post] = {
    val origin =
      if (originNode != null)
        originNode
      else
        node
    val (blame, check) = makeCheck(origin, label, extraSuppressor)
    currentBlame.having(blame) { Block(Seq(check, dispatch(body))) }
  }

  // appends Refute false after the statement
  private def appendCheck(
      node: Statement[Pre],
      label: String,
  ): Statement[Post] = {
    implicit val o: Origin = node.o
    val (blame, check) = makeCheck(node, label)
    Block(Seq(node.rewriteDefault(), currentBlame.having(blame) { check }))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    if (!doCheck || methodBlame.topOption.isEmpty || afterAssertFalse.nonEmpty)
      stat.rewriteDefault()
    else
      stat match {
        case branch @ Branch(
              Seq((cond, thenBody), (BooleanValue(true), elseBody))
            ) =>
          implicit val o: Origin = branch.o
          Branch(Seq(
            (
              dispatch(cond),
              instrumentBody(
                branch,
                s"then-branch (condition: `${condText(cond)}`)",
                thenBody,
                originNode = thenBody,
              ),
            ),
            (
              dispatch(BooleanValue(true)),
              instrumentBody(
                branch,
                s"else-branch (negation of condition: `${condText(cond)}`)",
                elseBody,
                originNode = elseBody,
              ),
            ),
          ))

        case branch @ Branch(Seq((cond, thenBody))) =>
          implicit val o: Origin = branch.o
          Branch(Seq((
            dispatch(cond),
            instrumentBody(
              branch,
              s"then-branch (condition: `${condText(cond)}`)",
              thenBody,
            ),
          )))

        case loop @ Loop(init, cond, update, contract, body) =>
          implicit val o: Origin = loop.o
          // suppress body/post-loop dead reports if the invariant itself failed
          var invNotEstablishedFired = false
          var invNotMaintainedFired = false
          val bodySuppress: () => Boolean = () => invNotEstablishedFired
          val postLoopSuppress: () => Boolean =
            () => invNotEstablishedFired || invNotMaintainedFired

          val (invInfo, dispatchedContract) =
            contract match {
              case li: LoopInvariant[Pre] =>
                val wrappedBlame =
                  new Blame[LoopInvariantFailure] {
                    override def blame(error: LoopInvariantFailure): Unit = {
                      error match {
                        case _: LoopInvariantNotEstablished =>
                          invNotEstablishedFired = true
                        case _: LoopInvariantNotMaintained =>
                          invNotMaintainedFired = true
                        case _ =>
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
          val (afterBlame, afterCheck) = makeCheck(
            loop,
            s"code after loop (condition: `${condText(cond)}`$invInfo)",
            extraSuppressor = postLoopSuppress,
          )
          Block(Seq(
            instrumentedLoop,
            currentBlame.having(afterBlame) { afterCheck },
          ))

        case block @ Block(stmts) =>
          implicit val o: Origin = block.o
          // skip instrumentation for code after assert false, it is intentiom
          val cutoffIdx = stmts.indexWhere {
            case Assert(BooleanValue(false)) => true
            case _ => false
          }
          if (cutoffIdx < 0) { Block(stmts.map(dispatch)) }
          else {
            val (before, rest) = stmts.splitAt(cutoffIdx + 1)
            Block(before.map(dispatch) ++ afterAssertFalse.having(()) {
              rest.map(dispatch)
            })
          }

        // skip inhales/assumes generated by lock encoding passes as they have their own Refute nodes
        case i @ Inhale(_)
            if i.o.find[NamePrefix].exists(_.prefix == "checkLockInvSat") =>
          i.rewriteDefault()
        case i @ Inhale(_)
            if i.o.find[NamePrefix].exists(_.prefix == "lockInv") =>
          i.rewriteDefault()
        case i @ Inhale(_)
            if i.o.find[NamePrefix].exists(_.prefix == "lockHeld") =>
          i.rewriteDefault()
        case a @ Assume(_)
            if a.o.find[NamePrefix].exists(_.prefix == "lockCommitted") =>
          a.rewriteDefault()

        case a @ Assume(BooleanValue(false)) =>
          implicit val o: Origin = a.o
          methodBlame.top.blame(DeadBranch(a, "assume false"))
          Inhale(ff)
        case i @ Inhale(BooleanValue(false)) =>
          methodBlame.top.blame(DeadBranch(i, "inhale false"))
          i.rewriteDefault()
        case a @ Assume(assn) =>
          appendCheck(a, s"code after assume `${condText(assn)}`")
        case i @ Inhale(res) =>
          appendCheck(i, s"code after inhale `${condText(res)}`")

        case c: SwitchCase[Pre] =>
          implicit val o: Origin = c.o
          val label =
            c match {
              case Case(pattern) =>
                s"switch case (pattern: `${condText(pattern)}`)"
              case DefaultCase() => "switch default case"
            }
          val (caseBlame, caseCheck) = makeCheck(c, label)
          Block(Seq(
            c.rewriteDefault(),
            currentBlame.having(caseBlame) { caseCheck },
          ))

        // atomic(inv) inhales inv's invariant the same way lock obj does; treat it
        // the same way so re-entrant/nested atomic on an already-held invariant
        // (a permission duplication, same shape as a double lock) is caught.
        case atomic @ ParAtomic(_, body) =>
          implicit val o: Origin = atomic.o
          atomic.rewrite(content =
            instrumentBody(atomic, "atomic block body", body)
          )

        case other => other.rewriteDefault()
      }

  override def dispatch(node: ParRegion[Pre]): ParRegion[Post] =
    if (methodBlame.topOption.isEmpty || !doCheck)
      node.rewriteDefault()
    else
      node match {
        case block @ ParBlock(_, _, _, _, _, content) =>
          implicit val o: Origin = block.o
          // par block creates a fresh scope with the range constraint
          block.rewrite(content =
            instrumentBody(block, "parallel block body", content)
          )
        case other => other.rewriteDefault()
      }
}
