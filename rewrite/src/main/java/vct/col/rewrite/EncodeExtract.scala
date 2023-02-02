package vct.col.rewrite
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.EncodeExtract.{ExtractMayNotReturn, ExtractedOnlyPost, FramedProofPostconditionFailed, FramedProofPreconditionFailed, LoopInvariantPreconditionFailed, ExtractMayNotJumpOut}
import vct.col.util.AstBuildHelpers.contract
import vct.result.VerificationError.UserError

case object EncodeExtract extends RewriterBuilder {
  override def key: String = "extract"
  override def desc: String = "Extract contracted nodes into separate methods"

  case class ExtractedOnlyPost(blame: Blame[PostconditionFailed]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case error: PostconditionFailed => blame.blame(error)
      case error: TerminationMeasureFailed =>
        PanicBlame("Extracted method cannot recurse by construction").blame(error)
      case error: ContextEverywhereFailedInPost =>
        PanicBlame("Extracted method has no context contract").blame(error)
      case error: SignalsFailed =>
        PanicBlame("Extracted method cannot throw?").blame(error)
      case error: ExceptionNotInSignals =>
        PanicBlame("Extracted method cannot throw?").blame(error)
    }
  }

  case class LoopInvariantPreconditionFailed(invariant: LoopInvariant[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      invariant.blame.blame(LoopInvariantNotEstablished(error.failure, invariant))
  }

  case class FramedProofPreconditionFailed(framedProof: FramedProof[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      framedProof.blame.blame(FramedProofPreFailed(error.failure, framedProof))
  }

  case class FramedProofPostconditionFailed(framedProof: FramedProof[_]) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      framedProof.blame.blame(FramedProofPostFailed(error.failure, framedProof))
  }

  case class ExtractMayNotReturn(extract: Extract[_], ret: Return[_]) extends UserError {
    override def code: String = "extractReturn"
    override def text: String =
      Origin.messagesInContext(Seq(
        ret.o -> "This return statement ...",
        extract.o -> "... crosses this extracted region",
      ))
  }

  case class ExtractMayNotJumpOut(extract: Extract[_], jump: Statement[_], to: LabelDecl[_]) extends UserError {
    override def code: String = "extractGoto"
    override def text: String =
      Origin.messagesInContext(Seq(
        jump.o -> "This statement ...",
        to.o -> "... may jump to this label ...",
        extract.o -> "... which crosses this extracted region",
      ))
  }
}

case class EncodeExtract[Pre <: Generation]() extends Rewriter[Pre] {
  def extracted(region: Extract[Pre],
                preBlame: Blame[PreconditionFailed],
                requires: Expr[Pre],
                body: Statement[Pre],
                ensures: Expr[Pre],
                postBlame: Blame[PostconditionFailed])(implicit o: Origin): InvokeProcedure[Post] = {

    body.collectFirst { case ret: Return[Pre] => ret }.foreach(ret => throw ExtractMayNotReturn(region, ret))

    val jumps = body.collect {
      case s @ Goto(Ref(label)) => label -> s
      case s @ Continue(Some(Ref(label))) => label -> s
      case s @ Break(Some(Ref(label))) => label -> s
    }

    val targets = body.collect { case Label(decl, _) => decl }.toSet

    for((label, s) <- jumps) {
      if(!targets.contains(label)) {
        throw ExtractMayNotJumpOut(region, s, label)
      }
    }

    val extract = vct.col.rewrite.util.Extract[Pre]()
    val newRequires = extract.extract(requires)
    val newEnsures = extract.extract(ensures)
    val newBody = extract.extract(body)

    val (typeMap, inMap, outMap) = extract.finish()

    val proc = globalDeclarations.declare(new Procedure[Post](
      returnType = TVoid(),
      args = variables.dispatch(inMap.keys.toSeq),
      outArgs = variables.dispatch(outMap.keys.toSeq),
      typeArgs = variables.dispatch(typeMap.keys.toSeq),
      body = Some(dispatch(newBody)),
      contract = contract(
        UnsafeDontCare.Satisfiability("It is acceptable that paths are not reachable in a program."),
        requires = UnitAccountedPredicate(dispatch(newRequires)),
        ensures = UnitAccountedPredicate(dispatch(newEnsures)),
        decreases = Some(DecreasesClauseNoRecursion()),
      ),
    )(ExtractedOnlyPost(postBlame)))

    InvokeProcedure[Post](
      proc.ref,
      args = inMap.values.map(dispatch).toSeq,
      outArgs = outMap.values.map(dispatch).toSeq,
      typeArgs = typeMap.values.map(dispatch).toSeq,
      givenMap = Nil, yields = Nil,
    )(NoContext(preBlame))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = stat match {
    case Extract(Label(decl, body)) =>
      Label(labelDecls.dispatch(decl), dispatch(Extract(body)(stat.o)))(stat.o)

    case extract@Extract(body) => body match {
      case Loop(_, _, _, blame@LoopInvariant(invariant, _), _) =>
        extracted(
          extract,
          LoopInvariantPreconditionFailed(blame),
          invariant,
          body,
          invariant,
          PanicBlame("Loop contract implies postcondition immediately"),
        )(extract.o)

      case proof@FramedProof(pre, body, post) =>
        extracted(
          extract,
          FramedProofPreconditionFailed(proof),
          pre,
          body,
          post,
          FramedProofPostconditionFailed(proof),
        )(extract.o)
    }

    case other => rewriteDefault(other)
  }
}
