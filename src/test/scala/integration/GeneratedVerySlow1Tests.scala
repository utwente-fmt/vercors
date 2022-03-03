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

class GeneratedVerySlow1Tests extends AnyFlatSpec with Matchers {

  it should "pass with silicon and examples/arrays/BinarySearch.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/BinarySearch.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/DutchNationalFlag.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/DutchNationalFlag.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/JavaArrayExamples.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/JavaArrayExamples.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/Transpose.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/Transpose.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/arrays/backward-dep-e1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/backward-dep-e1.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/basic-examples.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/basic-examples.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/forward-dep-noauto.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/forward-dep-noauto.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/forward-dep.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/forward-dep.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/kernel-example-v2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/kernel-example-v2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/kernel-example-v3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/kernel-example-v3.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/kernel-example.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/kernel-example.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/vector-add.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/vector-add.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
