package vct.col.rewrite.bip

import upickle.default._
import vct.col.ast.{BipComponent, BipTransition}

import scala.collection.{mutable => mut}

case object BIP {
  case class VerificationResults() {
    val transitionResults: mut.Map[BipTransition[_], BipVerificationResult] = mut.LinkedHashMap()
    val constructorResults: mut.Map[BipComponent[_], BipVerificationResult] = mut.LinkedHashMap()
    val componentToTransitions: mut.Map[BipComponent[_], Seq[BipTransition[_]]] = mut.LinkedHashMap()

    def nonEmpty: Boolean = transitionResults.nonEmpty || constructorResults.nonEmpty

    def setWith[T](e: T, m: mut.Map[T, BipVerificationResult], result: BipVerificationResult): Unit = m.get(e) match {
      case Some(Success) => m(e) = result
      case Some(_) => throw EncodeBip.OverwritingBipResultError()
      case None => throw EncodeBip.UnexpectedBipResultError()
    }

    def report(bt: BipTransition[_], result: BipVerificationResult): Unit = setWith(bt, transitionResults, result)
    def report(bt: BipComponent[_], result: BipVerificationResult): Unit = setWith(bt, constructorResults, result)

    def declareWith[T](e: T, m: mut.Map[T, BipVerificationResult]): Unit = m.get(e) match {
      case Some(_) => throw EncodeBip.UnexpectedBipResultError()
      case None => m(e) = Success
    }

    def declare(bc: BipComponent[_], bt: BipTransition[_]): Unit = {
      declareWith(bt, transitionResults)
      componentToTransitions(bc) = componentToTransitions.getOrElseUpdate(bc, Seq()) :+ bt
    }
    def declare(bt: BipComponent[_]): Unit = declareWith(bt, constructorResults)

    import Standalone._

    implicit def proofResultFromBool(b: Boolean): ProofResult = ProofResult.mk(b)

    def toStandalone(transition: BipTransition[_]): TransitionEntry = {
      val sig = transition.signature

      TransitionEntry(TransitionSignature(sig.portName, sig.sourceStateName, sig.targetStateName, sig.textualGuard),
        transitionResults(transition) match {
          case ComponentInvariantNotMaintained => TransitionResults(false, false, false)
          case UpdateFunctionFailure =>           TransitionResults(false, false, false)
          case StateInvariantNotMaintained =>     TransitionResults(true, false, false)
          case PostconditionNotVerified =>        TransitionResults(true, true, false)
          case Success =>                         TransitionResults(true, true, true)
        })
    }

    def constructorToStandalone(component: BipComponent[_]): ConstructorReport = constructorResults(component) match {
      case ComponentInvariantNotMaintained => ConstructorReport(false, false)
      case StateInvariantNotMaintained => ConstructorReport(true, false)
      case Success => ConstructorReport(true, true)

      // The following cases should not appear in a constructor context
      case PostconditionNotVerified => ???
      case UpdateFunctionFailure => ???
    }

    def toStandalone(): VerificationReport = {
      VerificationReport(mut.LinkedHashMap.from(componentToTransitions.toSeq.map { case (component, transitions) =>
        (component.fqn.mkString("."), ComponentReport(constructorToStandalone(component), transitions.map(toStandalone)))
      }))
    }
  }
  sealed trait BipVerificationResult
  case object Success extends BipVerificationResult
  case object UpdateFunctionFailure extends BipVerificationResult
  case object ComponentInvariantNotMaintained extends BipVerificationResult
  case object StateInvariantNotMaintained extends BipVerificationResult
  case object PostconditionNotVerified extends BipVerificationResult

  case object Standalone {
    case object ProofResult {
      def mk(b: Boolean): ProofResult = if(b) Proven else NotProven
    }
    sealed trait ProofResult
    case object Proven extends ProofResult
    case object NotProven extends ProofResult

    case class ConstructorReport(componentInvariant: ProofResult, stateInvariant: ProofResult)

    case class TransitionSignature(name: String, source: String, target: String, guard: Option[String])
    case class TransitionResults(componentInvariant: ProofResult, stateInvariant: ProofResult, postCondition: ProofResult)
    case class TransitionEntry(signature: TransitionSignature, results: TransitionResults)
    case class ComponentReport(constructor: ConstructorReport, transitions: Seq[TransitionEntry])

    case class VerificationReport(components: mut.LinkedHashMap[String, ComponentReport]) {
      def toJson(): String = upickle.default.write(this, 2)
    }

    object VerificationReport {
      implicit val rwProven: ReadWriter[ProofResult] = readwriter[String].bimap[ProofResult](
        { case Proven => "proven"; case NotProven => "not proven" },
        { provenStr => if(provenStr == "proven") Proven else NotProven }
      )
      implicit val rwConstructor: ReadWriter[ConstructorReport] = macroRW
      implicit val rwComponent: ReadWriter[ComponentReport] = macroRW
      implicit val rwTransitionReport: ReadWriter[TransitionResults] = macroRW
      implicit val rwTransitionEntry: ReadWriter[TransitionEntry] = macroRW

      implicit val rwTransitionSignature: ReadWriter[TransitionSignature] =
        // Custom encoding because we want to make the "guard" key absent if there is no guard.
        // Default encoding from upickle is to have an empty list, which is wrong for us
        readwriter[ujson.Value].bimap[TransitionSignature](
          { sig =>
            val o = ujson.Obj("name" -> sig.name, "source" -> sig.source, "target" -> sig.target)
            sig.guard.foreach(g => o.value.put("guard", g))
            o
          },
          { v =>
            val o = v.obj
            TransitionSignature(o("name").str, o("source").str, o("target").str, o.get("guard").strOpt)
          }
        )

      implicit val rwVerification: ReadWriter[VerificationReport] = {
        // Custom encoding because we want to convert a verificationreport into a dictionary with the keys being fqns,
        // and the entry being component reports. The default serializer puts that dict behind a "components" key, as
        // that is the default encoding for case classes. This works around that.
        readwriter[ujson.Value].bimap[VerificationReport](
          { report => ujson.Obj(report.components.map { case (k, v) => (k, writeJs(v)) }) },
          { v => VerificationReport(v.obj.value.map { case (k, v) => (k, read[ComponentReport](v)) }) }
        )
      }

      def fromJson(str: String): Either[Exception, VerificationReport] = {
        try {
          Right(read[VerificationReport](str))
        } catch {
          case e: Exception => Left(e)
        }
      }
    }
  }
}
