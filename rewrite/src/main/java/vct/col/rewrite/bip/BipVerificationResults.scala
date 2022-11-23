package vct.col.rewrite.bip

import vct.col.ast.{BipComponent, BipTransition}

import scala.collection.mutable

case object BIP {
  case class VerificationResults() {
    val transitionResults: mutable.Map[BipTransition[_], BipVerificationResult] = mutable.LinkedHashMap()
    val constructorResults: mutable.Map[BipComponent[_], BipVerificationResult] = mutable.LinkedHashMap()
    val componentToTransitions: mutable.Map[BipComponent[_], Seq[BipTransition[_]]] = mutable.LinkedHashMap()

    def nonEmpty: Boolean = transitionResults.nonEmpty || constructorResults.nonEmpty

    def setWith[T](e: T, m: mutable.Map[T, BipVerificationResult], result: BipVerificationResult): Unit = m.get(e) match {
      case Some(Success) => m(e) = result
      case Some(_) => throw EncodeBip.OverwritingBipResultError()
      case None => throw EncodeBip.UnexpectedBipResultError()
    }

    def report(bt: BipTransition[_], result: BipVerificationResult): Unit = setWith(bt, transitionResults, result)
    def report(bt: BipComponent[_], result: BipVerificationResult): Unit = setWith(bt, constructorResults, result)

    def declareWith[T](e: T, m: mutable.Map[T, BipVerificationResult]): Unit = m.get(e) match {
      case Some(_) => throw EncodeBip.UnexpectedBipResultError()
      case None => m(e) = Success
    }

    def declare(bc: BipComponent[_], bt: BipTransition[_]): Unit = {
      declareWith(bt, transitionResults)
      componentToTransitions(bc) = componentToTransitions.getOrElseUpdate(bc, Seq()) :+ bt
    }
    def declare(bt: BipComponent[_]): Unit = declareWith(bt, constructorResults)

    import Standalone._

    def toStandalone(transition: BipTransition[_]): (TransitionSignature, TransitionReport) = {
      val sig = transition.signature

      (TransitionSignature(sig.portName, sig.sourceStateName, sig.targetStateName, sig.textualGuard),
        transitionResults(transition) match {
          case ComponentInvariantNotMaintained => TransitionReport(false, false, false)
          case UpdateFunctionFailure =>           TransitionReport(false, false, false)
          case StateInvariantNotMaintained =>     TransitionReport(true, false, false)
          case PostconditionNotVerified =>        TransitionReport(true, true, false)
          case Success =>                         TransitionReport(true, true, true)
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
      VerificationReport(componentToTransitions.toSeq.map { case (component, transitions) =>
        val transitionReports = transitions.map(toStandalone)
        (component.fqn.mkString("."),
          ComponentReport(constructorToStandalone(component), transitionReports))
      })
    }
  }
  sealed trait BipVerificationResult
  case object Success extends BipVerificationResult
  case object UpdateFunctionFailure extends BipVerificationResult
  case object ComponentInvariantNotMaintained extends BipVerificationResult
  case object StateInvariantNotMaintained extends BipVerificationResult
  case object PostconditionNotVerified extends BipVerificationResult

  case object Standalone {
    // In this context, true == proven to hold, false == not proven to hold

    case class ConstructorReport(componentInvariant: Boolean, stateInvariant: Boolean)

    case class TransitionSignature(name: String, source: String, target: String, guard: Option[String])
    case class TransitionReport(componentInvariant: Boolean, stateInvariant: Boolean, postCondition: Boolean)

    case class ComponentReport(constructor: ConstructorReport, transitions: Seq[(TransitionSignature, TransitionReport)])

    case class VerificationReport(components: Seq[(String, ComponentReport)]) {
      def toMaps(): (Map[String, ConstructorReport], Map[String, Map[TransitionSignature, TransitionReport]]) = {
        val m1 = components.map { case (fqn, report) => fqn -> report.constructor }.toMap
        val m2 = components.map { case (fqn, report) => fqn -> report.transitions.toMap }.toMap
        (m1, m2)
      }

      def equalsModuloOrdering(other: VerificationReport): Boolean = toMaps() == other.toMaps()
    }

    def result(b: Boolean): String = if(b) "proven" else "not proven"

    def toJson(report: ConstructorReport): ujson.Obj = ujson.Obj(
      "componentInvariant" -> result(report.componentInvariant),
      "stateInvariant" -> result(report.stateInvariant)
    )

    def toJson(sig: TransitionSignature): ujson.Obj = {
      val obj = ujson.Obj(
        "name" -> sig.name,
        "source" -> sig.source,
        "target" -> sig.target,
      )
      sig.guard.foreach { guard => obj.value.addOne("guard", guard) }
      obj
    }

    def toJson(report: TransitionReport): ujson.Obj = ujson.Obj(
      "componentInvariant" -> result(report.componentInvariant),
      "stateInvariant" -> result(report.stateInvariant),
      "postCondition" -> result(report.postCondition),
    )

    def toJson(transitionReport: (TransitionSignature, TransitionReport)): ujson.Obj = ujson.Obj(
      "signature" -> toJson(transitionReport._1),
      "results" -> toJson(transitionReport._2)
    )

    def toJson(report: ComponentReport): ujson.Obj = ujson.Obj(
      "constructor" -> toJson(report.constructor),
      "transitions" -> report.transitions.map(toJson)
    )

    def toJson(report: VerificationReport, indent: Option[Int] = Some(2)): String = {
      ujson.write(ujson.Obj.from(report.components.map { case (fqn, report) =>
        fqn -> toJson(report)
      }), indent.getOrElse(-1))
    }

    def fromJson(input: String): Either[String, VerificationReport] = ujson.read(input) match {
      case obj: ujson.Obj => try { Right(verificationReportFromJson(obj)) } catch { case e: Exception => Left(e.getMessage) }
      case _ => Left("input json is not an object")
    }

    def verificationReportFromJson(obj: ujson.Obj): VerificationReport =
      VerificationReport(obj.value.toSeq.map { case (fqn, componentReport: ujson.Obj) =>
        fqn -> componentReportFromJson(componentReport)
      })

    def componentReportFromJson(obj: ujson.Obj): ComponentReport =
      ComponentReport(
        constructorReportFromJson(obj("constructor").asInstanceOf),
        transitionReportsFromJson(obj("transitions").asInstanceOf)
      )

    def resultFromJson(value: ujson.Value): Boolean = value.asInstanceOf[ujson.Str].value == "proven"

    def constructorReportFromJson(obj: ujson.Obj): ConstructorReport =
      ConstructorReport(
        resultFromJson(obj("componentInvariant")),
        resultFromJson(obj("stateInvariant"))
      )

    def transitionReportsFromJson(arr: ujson.Arr): Seq[(TransitionSignature, TransitionReport)] =
      arr.value.map { value =>
        val obj = value.asInstanceOf[ujson.Obj]
        (transitionSignatureFromJson(obj("signature").asInstanceOf), transitionReportFromJson(obj("results").asInstanceOf))
      }.toSeq

    def str(value: ujson.Value): String = value.asInstanceOf[ujson.Str].value

    def transitionSignatureFromJson(obj: ujson.Obj): TransitionSignature = TransitionSignature(
      name = str(obj("name")),
      source = str(obj("source")),
      target = str(obj("target")),
      guard = obj.value.get("guard").map(str)
    )

    def transitionReportFromJson(obj: ujson.Obj): TransitionReport = TransitionReport(
      componentInvariant = resultFromJson(obj("componentInvariant")),
      stateInvariant = resultFromJson(obj("stateInvariant")),
      postCondition = resultFromJson(obj("postCondition"))
    )
  }
}
