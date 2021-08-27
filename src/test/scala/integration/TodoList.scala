package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TodoList extends AnyFlatSpec with Matchers {

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
  //DONE: present All Output in junit report.
  //DONE: https://www.scalatest.org/scaladoc/3.0.5/org/scalatest/Checkpoints.html

  //DONE: make all test pass                                  Maandag mee verder
  //DONE: Hij kan CommandLineTesting niet vinden voor sbt.    Vraag Bob.
  //DONE: sections-reduced.c bestand ofzo.   Deze is gewoon traag.
  //DONE: maak een basic suite

  //DONE: verwijder alle documenten om te testen of er examples zijn die niet gebruikt worden.
  //DONE: Main even fixen
  //DONE: Fix ArrayNullValues
  //DONE: Merge dev in deze branch
  //Splits All tests naar AllSlowTest
  //maak github action  Een met slow tests en een met alle tests.
  //  mac doet alles behalve AllTests en AllSLowTests
  //  windows doet alles behalve AllTests en AllSLowTests
  //readme aanpassen met informatie.
  //Ga door de pull request heen
  //DONE: restore IntegrationTestHelper

  //NIET: document van Sophie bekijken.
  //LATER: beter junit report action https://github.com/marketplace/actions/xunit-viewer
  //https://github.com/marketplace/actions/test-reporter
  //https://github.com/marketplace?type=actions&query=JUnit+Report+
  //https://stackoverflow.com/questions/64539269/add-logging-to-scalatest-xml-report
  //https://github.com/scalatest/scalatest/issues/1183
  //LATER: create something for Veymont
  //LATER: Remove CommandLine code
  //DONE: Issues aanmaken.



  //Notes meeting Bob
  //Doel meeting. Pre code review/ presentatie
  //Presenteer de gegenereerde testen.
  //Dependence injectie opsplitsing van main
  //presenteer IntegrationTestHelper
  //Oplossing voor side effects
  //  van object naar class
  //  System
  //  Configuration

  //toekomst
  //NOTE:  1 pull request of meerdere    (Ja, geef aan wat er nodig is.) (Basic test suite wel uitsplitsen. Uitvoeren op Mac en Windows)
  //NOTE:  add tekst configuration to examples. (issue)
  //NOTE:  foutmelding github.

  //LATER:  report in github verbeteren.  https://github.com/marketplace/actions/test-reporter
  //LATER:  sbt test naar apart sbt test.
  //LATER:  add support voor veymont
  //LATER:  splitsen  Nee(aparte suites configureren.)  (Mogelijk in aparte classen.)
  //LATER:  remove commandlinecode(Deze willen we houden --stop-before-backend) en uit github action.
  //LATER:  Specifiekere error berichten bekijken.
}
