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

    def indent(s: String): String = {
      val indent = "  "
      indent + s.replace("\n", "\n" + indent)
    }

    def result(b: Boolean): String = if(b) "\"proven\"" else "\"not proven\""

    def toJson(report: ConstructorReport): String = {
      s"""{
         |  "componentInvariant": ${result(report.componentInvariant)},
         |  "stateInvariant": ${result(report.stateInvariant)}
         |}""".stripMargin
    }

    def toJson(sig: TransitionSignature): String = {
      val guard = sig.guard match {
        case Some(txt) => s""", "guard": "$txt" """
        case None => """// This transition had no guard"""
      }
      s"""{
         |  "name": "${sig.name}",
         |  "source": "${sig.source}",
         |  "target": "${sig.target}"
         |  $guard
         |}""".stripMargin
    }

    def toJson(report: TransitionReport): String = {
      s"""{
         |  "componentInvariant": ${result(report.componentInvariant)},
         |  "stateInvariant": ${result(report.stateInvariant)},
         |  "postCondition": ${result(report.postCondition)}
         |}""".stripMargin
    }

    def toJson(transitionReport: (TransitionSignature, TransitionReport)): String = {
      val (sig, report) = transitionReport
      s"""{
         |  "signature": ${indent(toJson(sig)).strip},
         |  "results": ${indent(toJson(report)).strip}
         |}""".stripMargin
    }

    def toJson(report: ComponentReport): String = {
      s"""{
          |  "<constructor>": ${indent(toJson(report.constructor)).strip},
          |  "transitions": [
          |${indent(indent(report.transitions.map(toJson).mkString(",\n")))}
          |  ]
          |}""".stripMargin

    }

    def toJson(report: VerificationReport): String = {
      val reportBlocks = report.components.map { case (fqn, report) =>
        s""""$fqn": ${toJson(report).strip}""".stripMargin
      }
      s"""{
         |${reportBlocks.map(indent).mkString("\n,")}
         |}
         |""".stripMargin
    }
  }
}
