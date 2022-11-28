package vct.test.integration.examples

import vct.col.rewrite.bip.BIP.Standalone.VerificationReport
import vct.options.types.{Backend, PathOrStd}
import vct.test.integration.helper.VercorsSpec
import org.scalactic.source

import java.nio.file.Paths

class TechnicalJavaBipSpec extends VercorsSpec {
  def fromExamples(s: String): PathOrStd.Path = PathOrStd.Path(Paths.get(s"examples/$s"))

  def failingTest(code: String, report: String, files: String*)(implicit pos: source.Position): Unit = bipTest(files, report, Seq(code))

  def passingTest(report: String, files: String*)(implicit pos: source.Position): Unit = bipTest(files, report, Nil)

  def bipTest(files: Seq[String], report: String, expectedCodes: Seq[String])(implicit pos: source.Position): Unit = {
    val reportPath = fromExamples(report)
    val expectedReport = VerificationReport.fromJson(reportPath.readToCompletion()).getOrElse(fail(s"Parse error, or could not find report at $reportPath"))
    val filesDesc = files.map(p => s"examples/${p}").mkString(", ")
    val verdict = if(expectedCodes.isEmpty) "pass" else "fail"
    val desc = s"JavaBIP test with files $filesDesc should $verdict with report examples/$report (using silicon)"

    def processResult(codes: Seq[String], report: Option[VerificationReport]): Unit = {
      val codeCheck = (expectedCodes, codes) match {
        case (a, b) if a == b => ""
        case (err +: _, Nil) => s"Expected $err but got pass."
        case (Nil, err +: _) => s"Expected pass but got $err."
        case (expectedErr, err +: _) => s"Expected $expectedErr but got $err."
      }

      val reportCheck = report match {
        case Some(report) if report == expectedReport => ""
        case Some(_) => "The reports differ."
        case None => "The report is missing."
      }

      val err = codeCheck + reportCheck
      if (err.nonEmpty) {
        fail(s"Test completed with an unexpected result. $err")
      }
    }

    registerGenericTest(desc, Backend.Silicon, files.map(fromExamples))(processResult)
  }

  failingTest("bipComponentInvariantNotEstablished:false",
    "technical/javabip/ComponentInvariantNotEstablished.json",
    "technical/javabip/ComponentInvariantNotEstablished.java"
  )
  failingTest("bipStateInvariantNotEstablished:false",
    "technical/javabip/StateInvariantNotEstablished.json",
    "technical/javabip/StateInvariantNotEstablished.java"
  )

  failingTest("bipComponentInvariantNotMaintained:false",
    "technical/javabip/ComponentInvariantNotMaintained.json",
    "technical/javabip/ComponentInvariantNotMaintained.java"
  )
  failingTest("bipStateInvariantNotMaintained:false",
    "technical/javabip/StateInvariantNotMaintained.json",
    "technical/javabip/StateInvariantNotMaintained.java"
  )
  failingTest("bipTransitionPostconditionFailure:false",
    "technical/javabip/TransitionPostconditionFailed.json",
    "technical/javabip/TransitionPostconditionFailed.java",
  )

  vercors should verify using silicon example "technical/javabip/BipGuardUnsatisfiablePrecondition.java"
  vercors should verify using silicon example "technical/javabip/BipGuardUsed.java"
  vercors should verify using silicon example "technical/javabip/BipGuardDataUsed.java"
}
