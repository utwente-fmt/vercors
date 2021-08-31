package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Span}


/*+
  These tests have been generated based on the old test framework. For new test I would recommend splitting it up in
  different classes and giving more descriptive names.
 */

class GeneratedSlow5Tests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(60000,Millis)

  it should "fail with silicon and examples/permissions/BadLoop1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/BadLoop1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/permissions/BadLoop2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/BadLoop2.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/Counter.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/Counter.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/permissions/IncrFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/IncrFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/IncrPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/IncrPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/MultiIncrement.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/MultiIncrement.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/permissions/SwapIntegerFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/SwapIntegerFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/SwapIntegerPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/SwapIntegerPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/predicates/ScaleInlinePredicate.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/ScaleInlinePredicate.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/predicates/minmax-list.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/minmax-list.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/append-prepend-sequence.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/append-prepend-sequence.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/remove-value.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/remove-value.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/sequence-constructors.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/sequence-constructors.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/slicing.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/slicing.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/sequential/BoogieExampleFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieExampleFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/BoogieTest.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieTest.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sets/set_comprehension.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sets/set_comprehension.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sets/subset.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sets/subset.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/witnesses/WandDemoSilver.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/WandDemoSilver.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
