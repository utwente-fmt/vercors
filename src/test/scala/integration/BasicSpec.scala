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
  //DONE: section-reduced.c bestand ofzo.
  //make all test pass
  //get it working in CI.

  //present All Output in junit report.
  //beter junit report action https://github.com/marketplace/actions/xunit-viewer
  //https://github.com/marketplace/actions/test-reporter
  //https://github.com/marketplace?type=actions&query=JUnit+Report+

  //Meeting having a discussion. Examples is the worst kind of documentation. Lets copy some examples to the wiki but
  //400 is unreadable. Examples is the illusion of documentation.
  //https://www.scalatest.org/scaladoc/3.0.5/org/scalatest/Checkpoints.html
  //create something for Veymont

  //Remove CommandLine code


  //Doel meeting. Pre code review/ presentatie
  //Presenteer de gegenereerde testen.
  //Dependence injectie opsplitsing van main
  //presenteer IntegrationTestHelper
  //Oplossing voor side effects
  //  van object naar class
  //  System
  //  Configuration

  //toekomst
  //  cache     Nee
  //  splitsen  Nee(aparte suites configureren.)
  //  1 pull request of meerdere
  //  add tekst configuration to examples.
  //  report in github verbeteren.
  //  add support voor veymont
  //  sbt test naar apart sbt test.
  //  remove commandlinecode(mijn code gebruikt --test-before-validation) en uit github aciton.
}
