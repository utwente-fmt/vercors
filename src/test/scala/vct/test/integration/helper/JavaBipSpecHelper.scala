package vct.test.integration.helper

import vct.options.types.{Backend, PathOrStd}

import java.nio.file.Paths
import org.scalactic.source
import vct.col.rewrite.bip.BIP.Standalone.VerificationReport

abstract class JavaBipSpecHelper extends VercorsSpec {
  def fromExamples(s: String): PathOrStd.Path = PathOrStd.Path(Paths.get(s"examples/$s"))

  def failingTest(code: String, report: String, files: String*)(implicit pos: source.Position): Unit = bipTest(files, report, Seq(code))
  def failingTest(codes: Seq[String], report: String, files: String*)(implicit pos: source.Position): Unit = bipTest(files, report, codes)

  def passingTest(report: String, files: String*)(implicit pos: source.Position): Unit = bipTest(files, report, Nil)

  def bipTest(files: Seq[String], report: String, expectedCodes: Seq[String])(implicit pos: source.Position): Unit = {
    val reportPath = fromExamples(report)
    val filesDesc = files.map(p => s"examples/${p}").mkString(", ")
    val verdict = if(expectedCodes.isEmpty) "pass" else "fail"
    val desc = s"JavaBIP test with files $filesDesc should $verdict with report examples/$report (using silicon)"

    def processResult(codes: Seq[String], report: Option[VerificationReport]): Unit = {
      val codeCheck = (expectedCodes, codes) match {
        case (a, b) if a == b => ""
        case (err +: _, Nil) => s"Expected $err but got pass."
        case (Nil, err +: _) => s"Expected pass but got $err."
        case (expectedErr, actualErr) => s"Expected ${expectedErr.mkString(", ")}, but got ${actualErr.mkString(", ")}."
      }

      val expectedReport = VerificationReport.fromJson(reportPath.readToCompletion()).getOrElse(fail(s"Parse error, or could not find report at $reportPath"))
      val reportCheck = report match {
        case Some(report) if report == expectedReport => ""
        case Some(_) => "The reports differ."
        case None => "The report is missing."
      }

      val err = Seq(codeCheck, reportCheck).mkString(" ")
      if (err.strip().nonEmpty) {
        fail(s"Test completed with an unexpected result. $err")
      }
    }

    registerGenericTest(desc, Backend.Silicon, files.map(fromExamples))(processResult)
  }

}
