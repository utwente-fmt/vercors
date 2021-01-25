package vct.integration

import hre.util.TestReport
import org.scalatest._
import vct.test.{Case, CommandLineTesting}

import scala.collection.mutable

// Would like to have this as an actual sbt integration test.
// But jacoco seems to ignore integration tests when using "testOnly", so we put it here for now.

//@WrapWith(classOf[ConfigMapWrapperSuite])
//// Would like to use ConfigMap here instead of Map[_, _], but does not work.
//// Luckily the map is fine for now.
//// See: https://github.com/scalatest/scalatest/issues/1704
//class ExamplesIntegrationTest(configMap : Map[String, String]) extends FlatSpec with Matchers {
//  print("Hmmm: " + configMap + "\n")
//
//  val testDir = configMap.get("test").getOrElse("examples")
//  val cases = CommandLineTesting.getCasesInDirs(testDir)
//  for ((name, kees) <- cases) {
//    if (kees.verdict == null) {
//      kees.verdict = TestReport.Verdict.Pass
//    }
//    ("" + name) should s"${kees.verdict.toString.toLowerCase}" in {
//      val report = checkCase(kees)
//      report.getVerdict should equal(kees.verdict)
//    }
//  }
//
//  def checkCase(kees: Case): TestReport = vct.main.Main.runVerification(kees.toArgs.toArray(new Array[String](0)))
//}

class ExamplesIntegrationTest extends WordSpec with Matchers {
  override def run(testName: Option[String], args: Args): Status = {
    val configMap = args.configMap

    println(configMap)
    val testDir = configMap.getWithDefault[String]("test", "examples/goto")
    println(testDir)
    val cases = CommandLineTesting.getCasesInDirs(testDir)
    for ((name, kees) <- cases) {
      if (kees.verdict == null) {
        kees.verdict = TestReport.Verdict.Pass
      }

      ("" + name) should {
        var report : TestReport = null
        s"${kees.verdict.toString.toLowerCase}" in {
          report = checkCase(kees)
          report.getVerdict should equal(kees.verdict)
        }
        s"${kees.verdict.toString.toLowerCase} the method ${name}.foo" in {
          report.getVerdict should equal(kees.verdict)
        }
      }
    }

    super.run(testName, args)
  }

  def checkCase(kees: Case): TestReport = vct.main.Main.runVerification(kees.toArgs.toArray(new Array[String](0)))
}
