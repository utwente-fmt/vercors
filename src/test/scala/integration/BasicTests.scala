package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Span}



class BasicTests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(10000,Millis)

  it should "error with silicon and examples/basic/ArrayAsObject.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/ArrayAsObject.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/basic/Boxing.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/Boxing.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/CollectionTest.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/CollectionTest.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/ContractSatisfiableIntentional.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/ContractSatisfiableIntentional.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/ContractSatisfiableIntentional.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/ContractSatisfiableIntentional.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/basic/ContractSatisfiableUnintentional.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/ContractSatisfiableUnintentional.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/basic/ContractSatisfiableUnintentional.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/ContractSatisfiableUnintentional.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/For.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/For.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/InlineFunctions.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/InlineFunctions.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/MethodCallNegation.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/MethodCallNegation.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/NewClassGhost.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/NewClassGhost.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/ReturnNull.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/ReturnNull.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/TernaryOperator.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/TernaryOperator.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/TurnOffContractSatisfiable.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/TurnOffContractSatisfiable.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.disableSat = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/bracket-issues.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/bracket-issues.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/basic/constructor-name.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/constructor-name.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/for.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/for.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/frac1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/frac1.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/frac2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/frac2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/fraction-comparison.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/fraction-comparison.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/given-in-par.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/given-in-par.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/postfix-increment.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/postfix-increment.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/basic/preceding-garbage.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/preceding-garbage.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/predicate.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/predicate.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/basic/predicate.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/predicate.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/basic/pure.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/pure.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/basic/seq-immutable.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/seq-immutable.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/seq-length.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/seq-length.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/seq-seq-length.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/seq-seq-length.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/basic/trailing-garbage.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/trailing-garbage.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/yield.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/yield.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
