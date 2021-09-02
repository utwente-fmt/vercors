package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/*+
  These tests have been generated based on the old test framework. For new test I would recommend splitting it up in
  different classes and giving more descriptive names.
 */

class GeneratedVerySlow4Tests extends AnyFlatSpec with Matchers {

  it should "pass with silicon and examples/histories/History.pvl and examples/histories/HistoryLoop.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/History.pvl", "examples/histories/HistoryLoop.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/histories/HistoryAppl.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/HistoryAppl.java", "examples/histories/HistoryJava.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/histories/TestHist.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/TestHist.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/layers/LFQ.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/layers/LFQ.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/layers/LFQHist.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/layers/LFQHist.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/lists/linkedlist.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/lists/linkedlist.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/array.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/array.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/fibonacci.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/fibonacci.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/maps/maps.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/maps/maps.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/openmp/sections-reduced-fail.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/sections-reduced-fail.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/openmp/add-spec.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/add-spec.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/openmp/parallel-example1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/parallel-example1.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/openmp/sections-reduced.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/sections-reduced.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/array_par.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/array_par.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/inv-test.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/inv-test.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/monotonicBool.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/monotonicBool.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/zero_mixed_array_2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/zero_mixed_array_2.pvl", "examples/parallel/zero-many.pvl", "examples/parallel/zero_array_ic.pvl", "examples/parallel/zero_matrix_ic.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }



}
