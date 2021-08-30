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

class GeneratedSlow3Tests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(60000,Millis)

  it should "pass with silicon and examples/dafny/DafnyIncr.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/dafny/DafnyIncr.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3a-func-with-lemmas.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3a-func-with-lemmas.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/demo/demo3a-func.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3a-func.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3a.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3a.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3b.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3b.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3c.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3c.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3d.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3d.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo4.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue5.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue5.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue7.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue7.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/floats/TestCountFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/floats/TestCountFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/floats/TestCountPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/floats/TestCountPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/floats/TestHist.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/floats/TestHist.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/forkjoin/forkjoininforloop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/forkjoin/forkjoininforloop.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/NoSendAfterRead.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/NoSendAfterRead.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/TestFuture.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/TestFuture.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/futures/TestFuturePermsFail.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/TestFuturePermsFail.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/TestFuturePermsPass.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/TestFuturePermsPass.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/counteradd_2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/counteradd_2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/counteradd_n.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/counteradd_n.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/locking.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/locking.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/unequalcounting.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/unequalcounting.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/LabeledWhile.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledWhile.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/LabeledWhile.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledWhile.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/gpgpu/simple-ltid.cu" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/gpgpu/simple-ltid.cu")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/graphs/reach.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/graphs/reach.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
