package viper.api
import vct.col.{ast => col, origin => blame}
import vct.result.VerificationResult.SystemError
import viper.silver.verifier.errors._
import viper.silver.verifier._
import viper.silver.{ast => silver}

trait SilverBackend extends Backend {
  case class NotSupported(text: String) extends SystemError
  case class ViperCrashed(text: String) extends SystemError

  def createVerifier: Verifier

  private def get[T <: col.Node[_]](node: silver.Infoed): T =
    node.info.asInstanceOf[NodeInfo[T]].node

  override def submit(program: col.Program[_]): Unit = {
    val silver = ColToSilver.transform(program)
    println(silver)
    createVerifier.verify(silver) match {
      case Success =>
      case Failure(errors) => errors.foreach {
        case err: AbstractVerificationError => err match {
          case Internal(node, reason, _) =>
            throw ViperCrashed(s"Viper returned an internal error at $node: $reason")
          case AssignmentFailed(node, reason, _) =>
            get[col.SilverAssign[_]](node) match {
              case fieldAssign@col.SilverFieldAssign(_, _, _) =>
                reason match {
                  case reasons.InsufficientPermission(access) if get[col.Node[_]](access) == fieldAssign =>
                    fieldAssign.blame.blame(blame.SilverAssignFailed(fieldAssign))
                  case otherReason =>
                    defer(otherReason)
                }
              case col.SilverLocalAssign(_, _) =>
                defer(reason)
            }
          case CallFailed(_, reason, _) =>
            defer(reason)
          case ContractNotWellformed(node, reason, _) =>
            defer(reason)
          case PreconditionInCallFalse(node, reason, _) =>
            val invocation = get[col.MethodInvocation[_]](node)
            invocation.blame.blame(blame.PreconditionFailed(getFailure(reason), invocation))
          case PreconditionInAppFalse(node, reason, _) =>
            val invocation = get[col.FunctionInvocation[_]](node)
            invocation.blame.blame(blame.PreconditionFailed(getFailure(reason), invocation))
          case ExhaleFailed(node, reason, _) =>
            val exhale = get[col.Exhale[_]](node)
            reason match {
              case reasons.InsufficientPermission(permNode) => get[col.Node[_]](permNode) match {
                case _: col.SilverResource[_] =>
                  exhale.blame.blame(blame.ExhaleFailed(getFailure(reason), exhale))
                case _ =>
                  defer(reason)
              }
              case reasons.AssertionFalse(_) | reasons.NegativePermission(_) | reasons. ReceiverNotInjective(_) =>
                exhale.blame.blame(blame.ExhaleFailed(getFailure(reason), exhale))
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
            val assert = get[col.Assert[_]](node)
            reason match {
              case reasons.InsufficientPermission(permNode) => get[col.Node[_]](permNode) match {
                case _: col.SilverResource[_] =>
                  assert.blame.blame(blame.AssertFailed(getFailure(reason), assert))
                case _ =>
                  defer(reason)
              }
              case reasons.AssertionFalse(_) | reasons.NegativePermission(_) | reasons.ReceiverNotInjective(_) =>
                assert.blame.blame(blame.AssertFailed(getFailure(reason), assert))
              case otherReason =>
                defer(otherReason)
            }
          case PostconditionViolated(_, member, reason, _) =>
            val applicable = get[col.ContractApplicable[_]](member)
            applicable.blame.blame(blame.PostconditionFailed(getFailure(reason), applicable))
          case FoldFailed(node, reason, _) =>
            val fold = get[col.SilverFold[_]](node)
            reason match {
              case reasons.InsufficientPermission(access) =>
                if(node.contains(access)) {
                  defer(reason)
                } else {
                  fold.blame.blame(blame.SilverFoldFailed(getFailure(reason), fold))
                }
              case reasons.AssertionFalse(_) | reasons.NegativePermission(_) | reasons.ReceiverNotInjective(_) =>
                fold.blame.blame(blame.SilverFoldFailed(getFailure(reason), fold))
              case otherReason =>
                defer(otherReason)
            }
          case UnfoldFailed(node, reason, _) =>
            reason match {
              case reasons.InsufficientPermission(access) =>
                get[col.Node[_]](access) match {
                  case col.SilverPredicateAccess(_, _, _) =>
                    val unfold = get[col.SilverUnfold[_]](node)
                    unfold.blame.blame(blame.SilverUnfoldFailed(getFailure(reason), unfold))
                  case _ =>
                    defer(reason)
                }
              case otherReason =>
                defer(otherReason)
            }
          case LoopInvariantNotPreserved(node, reason, _) =>
            val `while` = get[col.SilverWhile[_]](node)
            `while`.blame.blame(blame.SilverWhileInvariantNotMaintained(getFailure(reason), `while`))
          case LoopInvariantNotEstablished(node, reason, _) =>
            val `while` = get[col.SilverWhile[_]](node)
            `while`.blame.blame(blame.SilverWhileInvariantNotEstablished(getFailure(reason), `while`))
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

  def getFailure(reason: ErrorReason): blame.ContractFailure = reason match {
    case reasons.AssertionFalse(expr) => blame.ContractFalse(get[col.Expr[_]](expr))
    case reasons.InsufficientPermission(access) => blame.InsufficientPermissionToExhale(get[col.SilverResource[_]](access))
    case reasons.ReceiverNotInjective(access) => blame.ReceiverNotInjective(get[col.SilverResource[_]](access))
    case reasons.NegativePermission(p) => blame.NegativePermissionValue(p.info.asInstanceOf[NodeInfo[_]].permissionValuePermissionNode.get) // need to fetch access
  }

  def defer(reason: ErrorReason): Unit = reason match {
    case reasons.DivisionByZero(e) =>
      val division = get[col.DividingExpr[_]](e)
      division.blame.blame(blame.DivByZero(division))
    case reasons.InsufficientPermission(f@silver.FieldAccess(_, _)) =>
      val deref = get[col.SilverDeref[_]](f)
      deref.blame.blame(blame.InsufficientPermission(deref))
    case reasons.LabelledStateNotReached(expr) =>
      val old = get[col.Old[_]](expr)
      old.blame.blame(blame.LabelNotReached(old))
    case reasons.SeqIndexNegative(_, idx) =>
      val subscript = idx.info.asInstanceOf[NodeInfo[_]].seqIndexSubscriptNode.get
      subscript.blame.blame(blame.SeqBoundNegative(subscript))
    case reasons.SeqIndexExceedsLength(_, idx) =>
      val subscript = idx.info.asInstanceOf[NodeInfo[_]].seqIndexSubscriptNode.get
      subscript.blame.blame(blame.SeqBoundExceedsLength(subscript))
  }
}