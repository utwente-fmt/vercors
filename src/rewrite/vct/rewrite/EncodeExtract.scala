package vct.col.rewrite
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.EncodeExtract.{
  ExtractMayNotJumpOut,
  ExtractMayNotReturn,
  ExtractedOnlyPost,
  FramedProofPostconditionFailed,
  FramedProofPreconditionFailed,
  LoopInvariantPreconditionFailed,
  WrongExtractLoop,
  WrongExtractNode,
}
import vct.col.util.AstBuildHelpers.{ExprBuildHelpers, contract}
import vct.col.util.Substitute
import vct.result.Message
import vct.result.VerificationError.UserError

case object EncodeExtract extends RewriterBuilder {
  override def key: String = "extract"
  override def desc: String = "Extract contracted nodes into separate methods"

  case class WrongExtractNode(node: Statement[_]) extends UserError {
    override def code: String = "wrongExtract"
    override def text: String =
      node.o.messageInContext(
        "This kind of node cannot be extracted into a separate method"
      )
  }

  case class WrongExtractLoop(loop: Statement[_]) extends UserError {
    override def code: String = "wrongExtractLoop"
    override def text: String =
      loop.o.messageInContext(
        "This for loop can only be extracted when having format 'for(T i = target; ...; ...){"
      )
  }

  case class ExtractedOnlyPost(
      blame: Blame[PostconditionFailed],
      extract: Extract[_],
  ) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit =
      error match {
        case error: PostconditionFailed => blame.blame(error)
        case error: TerminationMeasureFailed =>
          if (extract.decreases.isDefined)
            extract.blame
              .blame(ExtractTerminationMeasureFailedClause(extract, error))
          else
            extract.blame
              .blame(ExtractTerminationMeasureFailedNoClause(extract))
        case error: ContextEverywhereFailedInPost =>
          PanicBlame("Extracted method has no context contract").blame(error)
        case error: SignalsFailed =>
          PanicBlame("Extracted method cannot throw?").blame(error)
        case error: ExceptionNotInSignals =>
          PanicBlame("Extracted method cannot throw?").blame(error)
      }
  }

  case class LoopInvariantPreconditionFailed(invariant: LoopInvariant[_])
      extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      invariant.blame
        .blame(LoopInvariantNotEstablished(error.failure, invariant))
  }

  case class FramedProofPreconditionFailed(framedProof: FramedProof[_])
      extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      framedProof.blame.blame(FramedProofPreFailed(error.failure, framedProof))
  }

  case class FramedProofPostconditionFailed(framedProof: FramedProof[_])
      extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      framedProof.blame.blame(FramedProofPostFailed(error.failure, framedProof))
  }

  case class ExtractMayNotReturn(extract: Extract[_], ret: Return[_])
      extends UserError {
    override def code: String = "extractReturn"
    override def text: String =
      Message.messagesInContext(
        ret.o -> "This return statement ...",
        extract.o -> "... crosses this extracted region",
      )
  }

  case class ExtractMayNotJumpOut(
      extract: Extract[_],
      jump: Statement[_],
      to: LabelDecl[_],
  ) extends UserError {
    override def code: String = "extractGoto"
    override def text: String =
      Message.messagesInContext(
        jump.o -> "This statement ...",
        to.o -> "... may jump to this label ...",
        extract.o -> "... which crosses this extracted region",
      )
  }
}

case class EncodeExtract[Pre <: Generation]() extends Rewriter[Pre] {
  def extracted(
      region: Extract[Pre],
      preBlame: Blame[PreconditionFailed],
      requires: Expr[Pre],
      body: Statement[Pre],
      ensures: Expr[Pre],
      postBlame: Blame[PostconditionFailed],
      decreases: Option[DecreasesClause[Pre]],
  )(implicit o: Origin): InvokeProcedure[Post] = {

    body.collectFirst { case ret: Return[Pre] => ret }
      .foreach(ret => throw ExtractMayNotReturn(region, ret))

    val jumps = body.collect {
      case s @ Goto(Ref(label)) => label -> s
      case s @ Continue(Some(Ref(label))) => label -> s
      case s @ Break(Some(Ref(label))) => label -> s
    }

    val targets = body.collect { case Label(decl, _, _) => decl }.toSet

    for ((label, s) <- jumps) {
      if (!targets.contains(label)) {
        throw ExtractMayNotJumpOut(region, s, label)
      }
    }

    val extract = vct.col.rewrite.util.Extract[Pre]()
    val newRequires = extract.extract(requires)
    val newEnsures = extract.extract(ensures)
    val newBody = extract.extract(body)
    val newDecreases = decreases.map(d => extract.extract(d))

    val extract.Data(typeMap, inMap, inForOutMap, outMap, inForOutAssign) =
      extract.finish()

    val fixMap = Substitute[Pre](inForOutMap.keys.map { case (in, out) =>
      Local[Pre](out.ref) -> Local[Pre](in.ref)
    }.toMap)
    val fixedRequires = fixMap.dispatch(newRequires)
    val fixedDecreases = newDecreases.map(fixMap.dispatch)

    val proc = globalDeclarations.declare(
      new Procedure[Post](
        returnType = TVoid(),
        args = variables
          .dispatch(inMap.keys.toSeq ++ inForOutMap.keys.map(_._1).toSeq),
        outArgs = variables.dispatch(outMap.keys.toSeq),
        typeArgs = variables.dispatch(typeMap.keys.toSeq),
        body = Some(Block(Seq(dispatch(inForOutAssign), dispatch(newBody)))),
        contract = contract(
          UnsafeDontCare.Satisfiability(
            "It is acceptable that paths are not reachable in a program."
          ),
          requires = UnitAccountedPredicate(dispatch(fixedRequires)),
          ensures = UnitAccountedPredicate(dispatch(newEnsures)),
          decreases = fixedDecreases.map(dispatch),
        ),
      )(ExtractedOnlyPost(postBlame, region))
    )

    InvokeProcedure[Post](
      proc.ref,
      args = (inMap.values ++ inForOutMap.values).map(dispatch).toSeq,
      outArgs = outMap.values.map(dispatch).toSeq,
      typeArgs = typeMap.values.map(dispatch).toSeq,
      givenMap = Nil,
      yields = Nil,
    )(NoContext(preBlame))
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] =
    stat match {
      case e @ Extract(Label(decl, body, contract), decreases) =>
        Label(
          labelDecls.dispatch(decl),
          dispatch(Extract(body, decreases)(e.blame)(stat.o)),
          dispatch(contract),
        )(stat.o)

      case extract @ Extract(body, decreases) =>
        body match {
          case Loop(init, cond, _, blame @ LoopInvariant(invariant, _), _) =>
            val targetValuePair =
              init match {
                // TODO: value must be a pure expression, we need to check this. It will now just error if this happens
                //  since the value is in a contract
                case Eval(PreAssignExpression(target, value)) =>
                  Some((target, value))
                case Block(Assign(target, value) +: Nil) =>
                  Some((target, value))
                case Block(Seq()) => None
                case _ => throw WrongExtractLoop(body)
              }
            val pre: Expr[Pre] =
              if (targetValuePair.isDefined)
                Star(
                  Eq(targetValuePair.get._1, targetValuePair.get._2)(init.o),
                  invariant,
                )(init.o)
              else
                invariant
            val result =
              extracted(
                extract,
                LoopInvariantPreconditionFailed(blame),
                pre,
                body,
                Star(invariant, Not(cond)(cond.o))(cond.o),
                PanicBlame("Loop contract implies postcondition immediately"),
                decreases,
              )(extract.o)
            if (targetValuePair.isDefined)
              Block[Post](Seq(dispatch(init), result))(init.o)
            else
              result
          case proof @ FramedProof(pre, body, post) =>
            extracted(
              extract,
              FramedProofPreconditionFailed(proof),
              pre,
              body,
              post,
              FramedProofPostconditionFailed(proof),
              decreases,
            )(extract.o)

          case other => throw WrongExtractNode(other)
        }

      case other => rewriteDefault(other)
    }
}
