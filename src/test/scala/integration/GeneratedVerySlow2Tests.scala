package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/*+
  These tests have been generated based on the old test framework. For new test I would recommend splitting it up in
  different classes and giving more descriptive names.
 */

class GeneratedVerySlow2Tests extends AnyFlatSpec with Matchers {

  it should "fail with silicon and examples/carp/forward-host.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/forward-host.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/forward-loop.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/forward-loop.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/histogram-submatrix.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/histogram-submatrix.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/summation-kernel-0.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/summation-kernel-0.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/summation-kernel-1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/summation-kernel-1.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/zero-sub-matrix.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/zero-sub-matrix.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchBreak.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchBreak.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchContinue.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchContinue.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchReturn.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchReturn.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyContinue.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyContinue.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/RosterFixed.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/RosterFixed.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/predicates/IntegerList.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/IntegerList.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/predicates/TreeRecursive.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/TreeRecursive.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/bubble-sort.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/bubble-sort.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/max-sort.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/max-sort.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/type-casts/CastExample.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/type-casts/CastExample.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/verifythis/2019/challenge1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/verifythis/2019/challenge1.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }



  it should "pass with silicon and examples/waitnotify/Main.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/waitnotify/Main.pvl", "examples/waitnotify/Worker.pvl", "examples/waitnotify/Queue.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/witnesses/ListAppend.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/ListAppend.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/witnesses/ListAppendASyncDef.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/ListAppendASyncDef.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/witnesses/ListAppendASyncDefInline.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/ListAppendASyncDefInline.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/witnesses/TreeRecursiveSilver.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/TreeRecursiveSilver.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/witnesses/TreeWandSilver-e1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/TreeWandSilver-e1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/witnesses/TreeWandSilver-e2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/TreeWandSilver-e2.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
