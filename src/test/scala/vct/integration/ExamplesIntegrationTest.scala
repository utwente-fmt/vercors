package vct.integration

import hre.util.TestReport
import org.scalatest._
import vct.test.{Case, CommandLineTesting}

// Would like to have this as an actual sbt integration test.
// But jacoco seems to ignore integration tests when using "testOnly", so we put it here for now.

@WrapWith(classOf[ConfigMapWrapperSuite])
// Would like to use ConfigMap here instead of Map[_, _], but does not work.
// Luckily the map is fine for now.
// See: https://github.com/scalatest/scalatest/issues/1704
class ExamplesIntegrationTest(configMap : Map[String, String]) extends FlatSpec with Matchers {
  print(configMap)
  val testDir = configMap.get("test").getOrElse("examples")
  val cases = CommandLineTesting.getCasesInDirs(testDir)
  for ((name, kees) <- cases) {
    if (kees.verdict == null) {
      kees.verdict = TestReport.Verdict.Pass
    }
    ("" + name) should s"${kees.verdict.toString.toLowerCase}" in {
      val report = checkCase(kees)
      report.getVerdict should equal(kees.verdict)
    }
  }

  def checkCase(kees: Case): TestReport = vct.main.Main.runVerification(kees.toArgs.toArray(new Array[String](0)))
}
