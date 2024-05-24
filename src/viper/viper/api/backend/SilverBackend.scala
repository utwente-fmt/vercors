package viper.api.backend

import com.typesafe.scalalogging.LazyLogging
import hre.io.RWFile
import hre.progress.Progress
import vct.col.origin.AccountedDirection
import vct.col.{ast => col, origin => blame}
import vct.result.VerificationError.SystemError
import viper.api.SilverTreeCompare
import viper.api.transform.{ColToSilver, NodeInfo, NopViperReporter, SilverParserDummyFrontend}
import viper.silver.ast.Infoed
import viper.silver.plugin.SilverPluginManager
import viper.silver.plugin.standard.termination.{FunctionTerminationError, LoopTerminationError, MethodTerminationError, TerminationConditionFalse, TupleBoundedFalse, TupleConditionFalse, TupleDecreasesFalse, TupleSimpleFalse}
import viper.silver.reporter.Reporter
import viper.silver.verifier._
import viper.silver.verifier.errors._
import viper.silver.{ast => silver}

import java.nio.file.{Files, Path}
import scala.reflect.ClassTag
import scala.util.{Try, Using}

trait SilverBackend extends Backend[(silver.Program, Map[Int, col.Node[_]])] with LazyLogging {
  case class NotSupported(text: String) extends SystemError
  case class ViperCrashed(text: String) extends SystemError

  case class ConsistencyErrors(errors: Seq[ConsistencyError]) extends SystemError {
    override def text: String =
      "The silver AST delivered to viper is not valid:\n" + errors.map(_.toString).mkString(" - ", "\n - ", "")
  }

  case class PluginErrors(errors: Seq[AbstractError]) extends SystemError {
    override def text: String =
      "An error occurred in a viper plugin, which should always be prevented:\n" + errors.map(_.toString).mkString(" - ", "\n - ", "")
  }

  case class NoInfo(node: silver.Infoed) extends SystemError {
    override def text: String = node match {
      case posed: silver.Positioned =>
        s"At ${posed.pos.toString}: node has no info (`$node`)"
      case _ =>
        s"node has no info (`$node`)"
    }
  }

  def createVerifier(reporter: Reporter, nodeFromUniqueId: Map[Int, col.Node[_]]): (Verifier, SilverPluginManager)
  def stopVerifier(verifier: Verifier): Unit

  private def info[T <: col.Node[_]](node: silver.Infoed)(implicit tag: ClassTag[T]): NodeInfo[T] =
    node.info.getAllInfos[NodeInfo[T]]
      .headOption.getOrElse(throw NoInfo(node))

  private def get[T <: col.Node[_]](node: silver.Infoed): T =
    info(node).node

  private def path(node: silver.Node): Seq[AccountedDirection] =
    info(node.asInstanceOf[silver.Infoed]).predicatePath.get

  def transform(colProgram: col.Program[_], output: Option[Path]): (silver.Program, Map[Int, col.Node[_]]) = {
    Progress.stages(Seq("Translation" -> 2, "Diagnostic Output" -> 2, "Check" -> 7, "Check Idempotency" -> 9)) { next =>
      val (silverProgram, nodeFromUniqueId) = ColToSilver.transform(colProgram)

      next()

      val silverProgramString =
        silverProgram
          .toString()
          .replace("requires decreases", "decreases")
          .replace("invariant decreases", "decreases")

      output.map(RWFile(_)).foreach(_.write { writer =>
        writer.write(silverProgramString)
      })

      next()

      silverProgram.checkTransitively match {
        case Nil =>
        case some => throw ConsistencyErrors(some)
      }

      next()

      val f = Files.createTempFile("vercors-", ".sil")
      try {
        Using(Files.newBufferedWriter(f))(_.write(silverProgramString))

        SilverParserDummyFrontend().parse(RWFile(f, doWatch = false)) match {
          case Left(errors) =>
            logger.warn("Possible viper bug: silver AST does not reparse when printing as text")
            for(error <- errors) {
              logger.warn(error.toString)
            }
          case Right(reparsedProgram) =>
            SilverTreeCompare.compare(silverProgram, reparsedProgram) match {
              case Nil =>
              case diffs =>
                logger.debug("Possible VerCors bug: reparsing the silver AST as text causes the AST to be different:")
                for((left, right) <- diffs) {
                  logger.debug(s" - Left: ${left.getClass.getSimpleName}: $left")
                  logger.debug(s" - Right: ${right.getClass.getSimpleName}: $right")
                }
            }
        }
      } finally {
        Files.delete(f)
      }

      (silverProgram, nodeFromUniqueId)
    }
  }

  override def submit(intermediateProgram: (silver.Program, Map[Int, col.Node[_]])): Boolean = {
    val (silverProgram, nodeFromUniqueId) = intermediateProgram
    val (verifier, plugins) = createVerifier(NopViperReporter, nodeFromUniqueId)

    try {
      val transformedProgram = plugins.beforeVerify(silverProgram) match {
        case Some(program) => program
        case None => throw PluginErrors(plugins.errors)
      }

      val backendVerifies =
        plugins.mapVerificationResult(transformedProgram, verifier.verify(transformedProgram)) match {
          case Success => true
          case Failure(errors) =>
            logger.debug(errors.toString())
            errors.foreach(processError)
            false
        }

      backendVerifies
    } finally {
      stopVerifier(verifier)
    }
  }

  def processError(error: AbstractError): Unit = error match {
    case err: AbstractVerificationError => err match {
      case Internal(node, reason, _) =>
        throw ViperCrashed(s"Viper returned an internal error at ${Try(node.toString()).getOrElse("?")}: $reason")
      case AssignmentFailed(node, reason, _) =>
        get[col.SilverAssign[_]](node) match {
          case fieldAssign@col.SilverFieldAssign(_, _, _) =>
            reason match {
              case reasons.InsufficientPermission(access) if get[col.Node[_]](access) == fieldAssign =>
                fieldAssign.blame.blame(blame.AssignFieldFailed(fieldAssign))
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
        val invocation = get[col.InvokeProcedure[_]](node)
        invocation.blame.blame(blame.PreconditionFailed(path(reason.offendingNode), getFailure(reason), invocation))
      case PreconditionInAppFalse(node, reason, _) =>
        val invocation = get[col.FunctionInvocation[_]](node)
        invocation.blame.blame(blame.PreconditionFailed(path(reason.offendingNode), getFailure(reason), invocation))
      case ExhaleFailed(node, reason, _) =>
        val exhale = get[col.Exhale[_]](node)
        reason match {
          case reasons.InsufficientPermission(permNode) => get[col.Node[_]](permNode) match {
            case _: col.Perm[_] | _: col.PredicateApply[_] | _: col.Value[_] =>
              exhale.blame.blame(blame.ExhaleFailed(getFailure(reason), exhale))
            case _ =>
              defer(reason)
          }
          case reasons.AssertionFalse(_) | reasons.NegativePermission(_) =>
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
            case _: col.Perm[_] | _: col.PredicateApply[_] | _: col.Value[_] =>
              assert.blame.blame(blame.AssertFailed(getFailure(reason), assert))
            case _ =>
              defer(reason)
          }
          case reasons.AssertionFalse(_) | reasons.NegativePermission(_) =>
            assert.blame.blame(blame.AssertFailed(getFailure(reason), assert))
          case otherReason =>
            defer(otherReason)
        }
      case PostconditionViolated(_, member, reason, _) =>
        val applicable = get[col.ContractApplicable[_]](member)
        applicable.blame.blame(blame.PostconditionFailed(path(reason.offendingNode), getFailure(reason), applicable))
      case FoldFailed(node, reason, _) =>
        val fold = get[col.Fold[_]](node)
        reason match {
          case reasons.InsufficientPermission(access) =>
            if(node.contains(access)) {
              defer(reason)
            } else {
              fold.blame.blame(blame.FoldFailed(getFailure(reason), fold))
            }
          case reasons.AssertionFalse(_) | reasons.NegativePermission(_) =>
            fold.blame.blame(blame.FoldFailed(getFailure(reason), fold))
          case otherReason =>
            defer(otherReason)
        }
      case UnfoldFailed(node, reason, _) =>
        reason match {
          case reasons.InsufficientPermission(access) =>
            get[col.Node[_]](access) match {
              case col.PredicateApply(_, _, _) =>
                val unfold = get[col.Unfold[_]](node)
                unfold.blame.blame(blame.UnfoldFailed(getFailure(reason), unfold))
              case _ =>
                defer(reason)
            }
          case otherReason =>
            defer(otherReason)
        }
      case LoopInvariantNotPreserved(node, reason, _) =>
        val invariant = info(node).invariant.get
        invariant.blame.blame(blame.LoopInvariantNotMaintained(getFailure(reason), invariant))
      case LoopInvariantNotEstablished(node, reason, _) =>
        val invariant = info(node).invariant.get
        invariant.blame.blame(blame.LoopInvariantNotEstablished(getFailure(reason), invariant))
      case FunctionNotWellformed(_, reason, _) =>
        defer(reason)
      case PredicateNotWellformed(_, reason, _) =>
        defer(reason)
      case FunctionTerminationError(node: Infoed, reason, _) =>
        val apply = get[col.Invocation[_]](node)
        apply.ref.decl.blame.blame(blame.TerminationMeasureFailed(apply.ref.decl, apply, getDecreasesClause(reason)))
      case MethodTerminationError(node: Infoed, reason, _) =>
        val apply = get[col.Invocation[_]](node)
        apply.ref.decl.blame.blame(blame.TerminationMeasureFailed(apply.ref.decl, apply, getDecreasesClause(reason)))
      case LoopTerminationError(node: Infoed, reason, _) =>
        val decreases = get[col.DecreasesClause[_]](node)
        info(node).invariant.get.blame.blame(blame.LoopTerminationMeasureFailed(decreases))
      case TerminationFailed(node, reason, cached) =>
        throw NotSupported(s"Vercors does not support termination measures from Viper")
      case PackageFailed(node, reason, _) =>
        val packageNode = get[col.WandPackage[_]](node)
        reason match {
          case reasons.AssertionFalse(_) | reasons.NegativePermission(_) =>
            packageNode.blame.blame(blame.PackageFailed(getFailure(reason), packageNode))
          case reasons.InsufficientPermission(permNode) =>
            get[col.Node[_]](permNode) match {
              case col.Perm(_, _) | col.PredicateApply(_, _, _) | col.Value(_) =>
                packageNode.blame.blame(blame.PackageFailed(getFailure(reason), packageNode))
              case _ =>
                defer(reason)
            }
          case _ =>
            defer(reason)
        }
      case ApplyFailed(node, reason, _) =>
        val applyNode = get[col.WandApply[_]](node)
        reason match {
          case reasons.AssertionFalse(_) | reasons.NegativePermission(_) =>
            applyNode.blame.blame(blame.WandApplyFailed(getFailure(reason),applyNode)) // take the blame
          case reasons.InsufficientPermission(permNode) =>
            get[col.Node[_]](permNode) match {
              case col.Perm(_, _) | col.PredicateApply(_, _, _) | col.Value(_) =>
                applyNode.blame.blame(blame.WandApplyFailed(getFailure(reason),applyNode)) // take the blame
              case _ =>
                defer(reason)
            }
          case reasons.MagicWandChunkNotFound(magicWand) =>
            applyNode.blame.blame(blame.WandApplyFailed(blame.InsufficientPermissionToExhale(get(magicWand)), applyNode))
          case _ =>
            defer(reason)
        }
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
      case other =>
        throw NotSupported(s"Viper returned an error that VerCors does not recognize: $other")
    }
    case AbortedExceptionally(throwable) =>
      throwable.printStackTrace()
      throw ViperCrashed(s"Viper has crashed: $throwable")
    case other =>
      throw NotSupported(s"Viper returned an error that VerCors does not recognize: $other")
  }

  def getFailure(reason: ErrorReason): blame.ContractFailure = reason match {
    case reasons.AssertionFalse(expr) => blame.ContractFalse(get[col.Expr[_]](expr))
    case reasons.InsufficientPermission(access) => blame.InsufficientPermissionToExhale(get[col.Expr[_]](access))
    case reasons.MagicWandChunkNotFound(wand) => blame.InsufficientPermissionToExhale(get[col.Expr[_]](wand))
    case reasons.NegativePermission(p) => blame.NegativePermissionValue(info(p).permissionValuePermissionNode.get) // need to fetch access
    case _ => ???
  }

  def getDecreasesClause(reason: ErrorReason): col.DecreasesClause[_] = reason match {
    case TerminationConditionFalse(node) =>
      throw NotSupported("Vercors does not support termination measure conditions from Viper")
    case TupleConditionFalse(_) =>
      throw NotSupported("Vercors does not support termination measure conditions from Viper")

    case TupleSimpleFalse(node: Infoed) =>
      // PB: simple == (not decreasing || not bounded)
      get[col.DecreasesClause[_]](node)
    case TupleDecreasesFalse(node: Infoed) => get[col.DecreasesClause[_]](node)
    case TupleBoundedFalse(node: Infoed) => get[col.DecreasesClause[_]](node)

    case other =>
      throw NotSupported(s"Viper returned an error reason that VerCors does not recognize: $other")
  }

  def defer(reason: ErrorReason): Unit = reason match {
    case reasons.DivisionByZero(e) =>
      val division = info(e).dividingExpr.get
      division.blame.blame(blame.DivByZero(division))
    case reasons.InsufficientPermission(f@silver.FieldAccess(_, _)) =>
      val deref = get[col.SilverDeref[_]](f)
      deref.blame.blame(blame.InsufficientPermission(deref))
    case reasons.InsufficientPermission(p@silver.PredicateAccess(_, _)) =>
      val unfolding = info(p).unfolding.get
      unfolding.blame.blame(blame.UnfoldFailed(getFailure(reason), unfolding))
    case reasons.QPAssertionNotInjective(access: silver.ResourceAccess) =>
      val starall = info(access).starall.get
      starall.blame.blame(blame.ReceiverNotInjective(starall, get(access)))
    case reasons.LabelledStateNotReached(expr) =>
      val old = get[col.Old[_]](expr)
      old.blame.blame(blame.LabelNotReached(old))
    case reasons.SeqIndexNegative(_, idx) =>
      val subscript = info(idx).seqIndexSubscriptNode.get
      subscript.blame.blame(blame.SeqBoundNegative(subscript))
    case reasons.SeqIndexExceedsLength(_, idx) =>
      val subscript = info(idx).seqIndexSubscriptNode.get
      subscript.blame.blame(blame.SeqBoundExceedsLength(subscript))
    case reasons.MapKeyNotContained(_, key) =>
      val get = info(key).mapGet.get
      get.blame.blame(blame.MapKeyError(get))

    case other =>
      throw NotSupported(s"Viper returned an error reason that VerCors does not recognize: $other")
  }
}