package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.Ignore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@Ignore
class ProblemFailTests extends AnyFlatSpec with Matchers {

  it should "error with silicon and examples/known-problems/basic/double-declare.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/basic/double-declare.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/carp/histogram-matrix.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/carp/histogram-matrix.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/carp/zero-sub-matrix-par.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/carp/zero-sub-matrix-par.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/case-studies/prefixsum-drf.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/case-studies/prefixsum-drf.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/classes/OnlyInstanceof.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/classes/OnlyInstanceof.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/domains/float.sil" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/domains/float.sil")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/futures/elect.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/futures/elect.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/inheritance/SimpleThreadMain.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/inheritance/SimpleThreadMain.java", "examples/known-problems/inheritance/SimpleThread.java", "examples/known-problems/inheritance/SimpleThreadInstance.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/layers/HistoryApplication.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/layers/HistoryApplication.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/layers/Java6Lock.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/layers/Java6Lock.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/openmp/addvec1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/openmp/addvec1.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/openmp/parvec.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/openmp/parvec.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/others/DuplicateFieldName.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/others/DuplicateFieldName.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/others/ParallelGCD.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/others/ParallelGCD.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/known-problems/others/QualifiedNames.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/others/QualifiedNames.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/others/array-problem.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/others/array-problem.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/others/rewriterIssue6.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/others/rewriterIssue6.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/others/summation.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/others/summation.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/parallel/monotonicBoolBroken.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/parallel/monotonicBoolBroken.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.stopBeforeBackend = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/permissions/SwapLong.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/permissions/SwapLong.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/permissions/SwapLongTwice.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/permissions/SwapLongTwice.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/known-problems/permissions/SwapLongWrong.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/permissions/SwapLongWrong.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/permissions/TreeStack.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/permissions/TreeStack.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/quickselect/ArrayPrinter.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/quickselect/ArrayPrinter.java", "examples/known-problems/quickselect/RandomBetween.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/quickselect/QuickSelect.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/quickselect/QuickSelect.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/known-problems/threads/VerifiedMain-E1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/threads/VerifiedMain-E1.java", "examples/known-problems/threads/Worker.java", "examples/known-problems/threads/SpecifiedThread.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/known-problems/threads/VerifiedMain-E2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/threads/VerifiedMain-E2.java", "examples/known-problems/threads/Worker.java", "examples/known-problems/threads/SpecifiedThread.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/threads/VerifiedMain.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/threads/VerifiedMain.java", "examples/known-problems/threads/Worker.java", "examples/known-problems/threads/SpecifiedThread.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/verifythis/2015/relaxed_prefix.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/verifythis/2015/relaxed_prefix.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/verifythis/2018/challenge1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/verifythis/2018/challenge1.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/known-problems/verifythis/2018/challenge2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/verifythis/2018/challenge2.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/verifythis/2019/challenge2a.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/verifythis/2019/challenge2a.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/verifythis/2019/challenge2b.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/verifythis/2019/challenge2b.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/known-problems/verifythis/2019/challenge3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/known-problems/verifythis/2019/challenge3.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
