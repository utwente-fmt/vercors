package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BasicSpec extends AnyFlatSpec with Matchers {

  //later
  // --debug vct.main.Main text aanpassen in program.
  //stop-after-typecheck deze word niet gebruikt
  //Could not find file komt vaak voor. Maar is wel een Pass.

  //todo
  //DONE split main
  //DONE stderr out should be allowed for testing
  //DONE listen in system
  //DONE make generic class for testing
  //DONE make integrationTestConfiguration
  //NAH create one place for all the settings (Maybe split it in multiple objects)
  //DONE Generate tests automatically
  //DONE Fix Veymont
  //DONE: skip problem-fail
  //DONE:java_DOT_lang_DOT_Object komt niet voor als je hem individueel draait
  //DONE: present All Output in junit report.
  //DONE: https://www.scalatest.org/scaladoc/3.0.5/org/scalatest/Checkpoints.html

  //make all test pass                                  Maandag mee verder
  //Hij kan CommandLineTesting niet vinden voor sbt.    Vraag Bob.
  //section-reduced.c bestand ofzo.                     Maandag mee verder

  //beter junit report action https://github.com/marketplace/actions/xunit-viewer
  //https://github.com/marketplace/actions/test-reporter
  //https://github.com/marketplace?type=actions&query=JUnit+Report+
  //https://stackoverflow.com/questions/64539269/add-logging-to-scalatest-xml-report
  //https://github.com/scalatest/scalatest/issues/1183

  //Meeting having a discussion. Examples is the worst kind of documentation. Lets copy some examples to the wiki but
  //400 examples is unreadable. Examples is the illusion of documentation.
  //create something for Veymont

  //Remove CommandLine code


}
