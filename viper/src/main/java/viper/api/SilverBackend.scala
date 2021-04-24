package viper.api
import vct.col.ast._
import vct.col.{ast => col}
import vct.result.VerificationResult.SystemError
import viper.silver.verifier.errors._
import viper.silver.verifier.reasons
import viper.silver.verifier._
import viper.silver.{ast => silver}

trait SilverBackend extends Backend {
  case class NotSupported(text: String) extends SystemError
  case class ViperCrashed(text: String) extends SystemError

  def createVerifier: Verifier

  private def get[T <: col.Node](node: silver.Infoed): T =
    node.info.asInstanceOf[NodeInfo[T]].node

  override def submit(program: Program): Unit = {
    val silver = ColToSilver.transform(program)
    println(silver)
    createVerifier.verify(silver) match {
      case Success =>
      case Failure(errors) => errors.foreach {
        case err: AbstractVerificationError => err match {
          case Internal(node, reason, _) =>
            throw ViperCrashed(s"Viper returned an internal error at $node: $reason")
          case AssignmentFailed(node, reason, _) =>
            get[SilverAssign](node) match {
              case fieldAssign@SilverFieldAssign(_, _, _) =>
                reason match {
                  case reasons.InsufficientPermission(access) if get[Node](access) == fieldAssign =>
                    fieldAssign.blame.silverAssignFailed(fieldAssign)
                  case otherReason =>
                    defer(otherReason)
                }
              case SilverLocalAssign(_, _) =>
                defer(reason)
            }
          case CallFailed(_, reason, _) =>
            defer(reason)
          case ContractNotWellformed(_, reason, _) =>
            defer(reason)
          case PreconditionInCallFalse(node, reason, _) =>
            val invocation = get[MethodInvocation](node)
            invocation.blame.preconditionFailed(getFailure(reason), invocation)
          case PreconditionInAppFalse(node, reason, _) =>
            val invocation = get[FunctionInvocation](node)
            invocation.blame.preconditionFailed(getFailure(reason), invocation)
          case ExhaleFailed(node, reason, _) =>
            val exhale = get[Exhale](node)
            reason match {
              case reasons.InsufficientPermission(permNode) => get[Node](permNode) match {
                case _: SilverResource =>
                  exhale.blame.exhaleFailed(getFailure(reason), exhale)
                case _ =>
                  defer(reason)
              }
              case reasons.AssertionFalse(_) | reasons.NegativePermission(_) | reasons. ReceiverNotInjective(_) =>
                exhale.blame.exhaleFailed(getFailure(reason), exhale)
              case otherReason =>
                defer(otherReason)
            }
          case InhaleFailed(node, reason, _) =>
            defer(reason)
          case IfFailed(node, reason, _) =>
            defer(reason)
          case WhileFailed(node, reason, _) =>
            defer(reason)
          case AssertFailed(node, reason, _) =>
            val assert = get[Assert](node)
            reason match {
              case reasons.InsufficientPermission(permNode) => get[Node](permNode) match {
                case _: SilverResource =>
                  assert.blame.assertFailed(getFailure(reason), assert)
                case _ =>
                  defer(reason)
              }
              case reasons.AssertionFalse(_) | reasons.NegativePermission(_) | reasons.ReceiverNotInjective(_) =>
                assert.blame.assertFailed(getFailure(reason), assert)
              case otherReason =>
                defer(otherReason)
            }
          case PostconditionViolated(_, member, reason, _) =>
            val applicable = get[ContractApplicable](member)
            applicable.blame.postconditionFailed(getFailure(reason), applicable)
          case FoldFailed(node, reason, _) =>
            val fold = get[SilverFold](node)
            reason match {
              case reasons.InsufficientPermission(access) =>
                if(node.contains(access)) {
                  defer(reason)
                } else {
                  fold.blame.silverFoldFailed(getFailure(reason), fold)
                }
              case reasons.AssertionFalse(_) | reasons.NegativePermission(_) | reasons.ReceiverNotInjective(_) =>
                fold.blame.silverFoldFailed(getFailure(reason), fold)
              case otherReason =>
                defer(otherReason)
            }
          case UnfoldFailed(node, reason, _) =>
            reason match {
              case reasons.InsufficientPermission(access) =>
                get[Node](access) match {
                  case SilverPredicateAccess(_, _, _) =>
                    val unfold = get[SilverUnfold](node)
                    unfold.blame.silverUnfoldFailed(getFailure(reason), unfold)
                  case _ =>
                    defer(reason)
                }
              case otherReason =>
                defer(otherReason)
            }
          case LoopInvariantNotPreserved(node, reason, _) =>
            val `while` = get[SilverWhile](node)
            `while`.blame.silverWhileInvariantNotMaintained(getFailure(reason), `while`)
          case LoopInvariantNotEstablished(node, reason, _) =>
            val `while` = get[SilverWhile](node)
            `while`.blame.silverWhileInvariantNotEstablished(getFailure(reason), `while`)
          case FunctionNotWellformed(_, reason, _) =>
            defer(reason)
          case PredicateNotWellformed(_, reason, _) =>
            defer(reason)
          case TerminationFailed(_, _, _) =>
            throw NotSupported(s"Vercors does not support termination measures from Viper")
          case PackageFailed(node, reason, _) =>
            throw NotSupported(s"Vercors does not support magic wands from Viper")
          case ApplyFailed(node, reason, _) =>
            throw NotSupported(s"Vercors does not support magic wands from Viper")
          case MagicWandNotWellformed(_, _, _) =>
            throw NotSupported(s"Vercors does not support magic wands from Viper")
          case LetWandFailed(_, _, _) =>
            throw NotSupported(s"Vercors does not support magic wands from Viper")
          case HeuristicsFailed(_, _, _) =>
            throw NotSupported(s"Vercors does not support magic wands from Viper")
          case ErrorWrapperWithExampleTransformer(_, _) =>
            throw NotSupported(s"Vercors does not support counterexamples from Viper")
          case VerificationErrorWithCounterexample(_, _, _, _, _) =>
            throw NotSupported(s"Vercors does not support counterexamples from Viper")
        }
        case AbortedExceptionally(throwable) =>
          throw ViperCrashed(s"Viper has crashed: $throwable")
        case other =>
          throw NotSupported(s"Viper returned an error that VerCors does not recognize: $other")
      }
    }
  }

  def getFailure(reason: ErrorReason): ContractFailure = reason match {
    case reasons.AssertionFalse(expr) => ContractFalse(get[Expr](expr))
    case reasons.InsufficientPermission(access) => InsufficientPermissionToExhale(get[SilverResource](access))
    case reasons.ReceiverNotInjective(access) => ReceiverNotInjective(get[SilverResource](access))
    case reasons.NegativePermission(p) => NegativePermissionValue(p.info.asInstanceOf[NodeInfo[_]].permissionValuePermissionNode.get) // need to fetch access
  }

  def defer(reason: ErrorReason): Unit = reason match {
    case reasons.DivisionByZero(e) =>
      val division = get[DividingExpr](e)
      division.blame.divisionByZero(division)
    case reasons.InsufficientPermission(f@silver.FieldAccess(_, _)) =>
      val deref = get[SilverDeref](f)
      deref.blame.silverInsufficientPermission(deref)
    case reasons.LabelledStateNotReached(expr) =>
      val old = get[Old](expr)
      old.blame.labelNotReached(old)
    case reasons.SeqIndexNegative(_, idx) =>
      val subscript = idx.info.asInstanceOf[NodeInfo[_]].seqIndexSubscriptNode.get
      subscript.blame.seqBoundNegative(subscript)
    case reasons.SeqIndexExceedsLength(_, idx) =>
      val subscript = idx.info.asInstanceOf[NodeInfo[_]].seqIndexSubscriptNode.get
      subscript.blame.seqBoundExceedsLength(subscript)
  }
}
