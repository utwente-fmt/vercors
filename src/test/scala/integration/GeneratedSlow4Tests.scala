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

class GeneratedSlow4Tests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(60000,Millis)

  it should "pass with silicon and examples/histories/History.pvl with check defined" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/History.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkDefined = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/histories/History.pvl with check Axioms" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/History.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkAxioms = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/OwickiGries.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/OwickiGries.pvl", "examples/manual/Worker.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/functions.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/functions.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/list.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/list.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/loop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/loop.pvl", "examples/manual/permissions.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/manual/main.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/main.pvl", "examples/manual/permissions.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/manual/permissions.pvl and examples/manual/parameters1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/permissions.pvl", "examples/manual/parameters1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/permissions.pvl and examples/manual/parameters2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/permissions.pvl", "examples/manual/parameters2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/zero_array.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/zero_array.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/maps/maps_example_from_dafny.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/maps/maps_example_from_dafny.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
  it should "pass with silicon and examples/openmp/copy-spec.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/copy-spec.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/openmp/zero-spec.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/zero-spec.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/InvariantParallelAtomic.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/InvariantParallelAtomic.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/ParBothRead.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParBothRead.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/ParNestedInvariant.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParNestedInvariant.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/block-par.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/block-par.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/parallel/inv-test-fail1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/inv-test-fail1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/parallel/inv-test-fail2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/inv-test-fail2.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
