package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/*
  These tests have been generated based on the old test framework. For new test I would recommend splitting it up in
  different classes and giving more descriptive names.
 */
class AllTests extends AnyFlatSpec with Matchers {

  "VerCors" should "pass with silicon and examples/abrupt/Abrupt.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/Abrupt.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/BadCustomException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/BadCustomException.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/CatchTypesIdenticalBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/CatchTypesIdenticalBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/CatchTypesSubtypeBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/CatchTypesSubtypeBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/abrupt/ContinueBreakFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ContinueBreakFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ContinueBreakPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ContinueBreakPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/DoubleNestedFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/DoubleNestedFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ExceptionAbortsAssignment.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ExceptionAbortsAssignment.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ExceptionsAndAbrupt.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ExceptionsAndAbrupt.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/Finally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/Finally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/GoodCustomException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/GoodCustomException.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/abrupt/KeYAbruptTerminationChallengeCarbon.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/KeYAbruptTerminationChallengeCarbon.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/abrupt/KeYAbruptTerminationChallengeSilicon.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/KeYAbruptTerminationChallengeSilicon.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/KnuthTabulate.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/KnuthTabulate.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/LabeledStatements.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/LabeledStatements.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/NestedTryCatchFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/NestedTryCatchFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/NoResultInSignals.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/NoResultInSignals.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OnlyCatch.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OnlyCatch.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OnlyReturn.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OnlyReturn.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OnlyThrows.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OnlyThrows.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OverloadedReturn.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OverloadedReturn.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/PureSignalsBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/PureSignalsBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/PureThrowsBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/PureThrowsBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ReturnFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ReturnFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/SignalsMustExtendThrowable.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SignalsMustExtendThrowable.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/abrupt/SignalsMustPropagate.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SignalsMustPropagate.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/SignalsThrowNoThrows.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SignalsThrowNoThrows.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/SwitchVarious.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SwitchVarious.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/Synchronized.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/Synchronized.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/ThrowIntBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ThrowIntBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ThrowsAndThrow.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ThrowsAndThrow.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/TypecheckSignalsBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckSignalsBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/TypecheckSignalsOk.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckSignalsOk.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/TypecheckThrowsMethodBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckThrowsMethodBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/TypecheckThrowsThrowBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckThrowsThrowBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/TypecheckTryCatch.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckTryCatch.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/UnusedCheckedCatchBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/UnusedCheckedCatchBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/WhileBreakFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/WhileBreakFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/WhileFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/WhileFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/WhileInFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/WhileInFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

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

  it should "pass with silicon and examples/arrays/array-example.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/array-example.pvl")
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

  it should "fail with silicon and examples/arrays/forward-dep-e1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/forward-dep-e1.c")
    configuration.verdict = Verdict.Fail
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

  it should "fail with silicon and examples/arrays/zero-array-ic-e1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/zero-array-ic-e1.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/AddAssignJava.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/AddAssignJava.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/basic/ArrayAsObject.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/ArrayAsObject.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/BasicArray.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BasicArray.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/basic/BasicAssert-e1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BasicAssert-e1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/BasicAssert.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BasicAssert.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/BooleanOperators.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BooleanOperators.java")
    configuration.verdict = Verdict.Pass
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

  it should "pass with silicon and examples/basic/InvokationInGuard.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/InvokationInGuard.java")
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

  it should "pass with silicon and examples/basic/MultiDimArray.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/MultiDimArray.java")
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

  it should "pass with silicon and examples/basic/array-item-access.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/array-item-access.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
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

  it should "pass with silicon and examples/basic/pointer.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/pointer.c")
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

  it should "pass with silicon and examples/basic/pvl-array.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/pvl-array.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/satcheck-check.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/satcheck-check.pvl")
    configuration.verdict = Verdict.Pass
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

  it should "pass with silicon and examples/basic/seq-item-access.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/seq-item-access.pvl")
    configuration.verdict = Verdict.Pass
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

  it should "pass with silicon and examples/basic/sumints.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/sumints.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/test-1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/test-1.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/basic/test-scale.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/test-scale.java")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/carp/ZeroArrayIC.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/ZeroArrayIC.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/carp/access-sub-matrix-fail.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/access-sub-matrix-fail.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/access-sub-matrix-pass.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/access-sub-matrix-pass.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

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

  it should "pass with silicon and examples/carp/zero-kernel.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/zero-kernel.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/zero-loop.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/zero-loop.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/zero-sub-array.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/zero-sub-array.c")
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

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchEmpty.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchEmpty.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchIf.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchIf.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchLog.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchLog.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchNestedTry.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchNestedTry.java")
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

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchStackTrace.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchStackTrace.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchSwitch.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchSwitch.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchThrowE.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchThrowE.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchThrowNewE.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchThrowNewE.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchThrowNewEWrap.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchThrowNewEWrap.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchWhile.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchWhile.java")
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

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyEmpty.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyEmpty.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyLog.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyLog.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyNestedTry.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyNestedTry.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyReturn.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyReturn.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyThrowNewE.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyThrowNewE.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/clang/c-example-impl.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/clang/c-example-impl.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/clang/c-example-use.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/clang/c-example-use.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/DifferentClassesDifferentTypes1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/DifferentClassesDifferentTypes1.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/classes/DifferentClassesDifferentTypes2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/DifferentClassesDifferentTypes2.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/classes/InnerClass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/InnerClass.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/NewException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/NewException.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/NewRuntimeException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/NewRuntimeException.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/OnlyClass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/OnlyClass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/OnlyExtends.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/OnlyExtends.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/OnlyNew.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/OnlyNew.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/Overloading.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/Overloading.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/dafny/DafnyIncr.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/dafny/DafnyIncr.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/dafny/DafnyIncrE1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/dafny/DafnyIncrE1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/demo/demo1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo1.pvl")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/domains/list.sil" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/domains/list.sil")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/domains/option.sil" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/domains/option.sil")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/constructor_requires_this.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/constructor_requires_this.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/not_a_location1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/not_a_location1.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/not_a_location2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/not_a_location2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/type_error_1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/type_error_1.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/type_error_2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/type_error_2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/executable/src/separate/Main.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/executable/src/separate/Main.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/executable/src/separate/Util.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/executable/src/separate/Util.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/fixed-known-problems/FunctionProblemFixed1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/FunctionProblemFixed1.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/fixed-known-problems/FunctionProblemFixed2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/FunctionProblemFixed2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue2.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue3.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue4.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
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

  it should "fail with silicon and examples/forkjoin/forkfail.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/forkjoin/forkfail.pvl")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/forkjoin/forkpass.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/forkjoin/forkpass.pvl")
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

  it should "error with silicon and examples/goto/BadLabeledIfWithContract.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/BadLabeledIfWithContract.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/LabeledIf.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledIf.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/LabeledIf.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledIf.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
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

  it should "fail with silicon and examples/goto/goto1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with carbon and examples/goto/goto1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/goto2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/goto2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/goto3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto3.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/goto3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto3.pvl")
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

  it should "pass with silicon and examples/histories/History.pvl and examples/histories/HistoryLoop.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/History.pvl", "examples/histories/HistoryLoop.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

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

  it should "error with silicon and examples/manual/BadType.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/BadType.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/manual/BadType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/BadType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/OwickiGries.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/OwickiGries.pvl", "examples/manual/Worker.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/manual/TestFork.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/TestFork.pvl")
    configuration.verdict = Verdict.Error
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

  it should "pass with silicon and examples/manual/functions.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/functions.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/induction-lemma.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/induction-lemma.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/manual/induction-problem.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/induction-problem.pvl")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/manual/option.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/option.pvl")
    configuration.verdict = Verdict.Pass
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

  it should "pass with silicon and examples/manual/permissions.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/permissions.pvl")
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

  it should "pass with silicon and examples/maps/maps.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/maps/maps.pvl")
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

  it should "pass with silicon and examples/openmp/add-spec.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/add-spec.c")
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

  it should "pass with silicon and examples/openmp/parallel-example1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/parallel-example1.pvl")
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

  it should "pass with silicon and examples/openmp/sections-reduced.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/sections-reduced.c")
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

  it should "pass with silicon and examples/parallel/ForWithinParallel.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ForWithinParallel.pvl")
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

  it should "error with silicon and examples/parallel/ParBothWrite.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParBothWrite.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/parallel/ParIterWrite.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParIterWrite.pvl")
    configuration.verdict = Verdict.Error
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

  it should "error with silicon and examples/parallel/ParNestedInvariantWrite.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParNestedInvariantWrite.pvl")
    configuration.verdict = Verdict.Error
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

  it should "pass with silicon and examples/permissions/RosterFixed.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/RosterFixed.java")
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

  it should "pass with silicon and examples/permissions/box.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/box.pvl")
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

  it should "error with silicon and examples/predicates/MutuallyRecursiveInlinePredicates.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/MutuallyRecursiveInlinePredicates.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/predicates/RecursiveInlinePredicate.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/RecursiveInlinePredicate.pvl")
    configuration.verdict = Verdict.Error
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

  it should "pass with silicon and examples/predicates/TreeRecursive.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/TreeRecursive.java")
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

  it should "pass with silicon and examples/refute/Sat.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/Sat.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/Unsat.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/Unsat.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/frame_error_1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/frame_error_1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/refute/refute1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute1.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/refute3.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute3.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/refute/refute4.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute4.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/refute5.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute5.java")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/sequences/updating.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/updating.pvl")
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

  it should "pass with silicon and examples/sequential/BoogieExamplePass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieExamplePass.java")
    configuration.verdict = Verdict.Pass
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

  it should "fail with silicon and examples/sequential/BoogieTestFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieTestFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/sequential/LoopInvFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/LoopInvFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/LoopInvPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/LoopInvPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/SimpleExamples.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/SimpleExamples.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/nested-loops.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/nested-loops.pvl")
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

  it should "pass with silicon and examples/technical/keywords/allowed-c.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-c.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/technical/keywords/allowed-c.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-c.c")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/technical/keywords/allowed-java.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-java.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/technical/keywords/allowed-java.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-java.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/technical/keywords/disallowed-c-inline.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-c-inline.c")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with carbon and examples/technical/keywords/disallowed-c-inline.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-c-inline.c")
    configuration.verdict = Verdict.Error
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/technical/keywords/disallowed-java-assert.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-java-assert.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with carbon and examples/technical/keywords/disallowed-java-assert.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-java-assert.java")
    configuration.verdict = Verdict.Error
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/technical/test-value1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/test-value1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/technical/test-value2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/test-value2.java")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/type-casts/TypeExample1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/type-casts/TypeExample1.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/type-casts/cast-example.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/type-casts/cast-example.pvl")
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

  it should "pass with veymont and examples/veymont-check/checkLTS/ltstest.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/ltstest.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkLTS/simpleifelse.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/simpleifelse.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkLTS/simplemethodcall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/simplemethodcall.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkLTS/simplewhile.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/simplewhile.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/MainConstructorArg.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/MainConstructorArg.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMain.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMain.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMainConstructor.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMainConstructor.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMainMethod.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMainMethod.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMainMethod2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMainMethod2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoRunArg.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoRunArg.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoRunMethod.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoRunMethod.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/OtherMainConstructor.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/OtherMainConstructor.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/TwoMainConstructors.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/TwoMainConstructors.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/WrongRunType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/WrongRunType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorAssignmentWrongType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorAssignmentWrongType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorAssignsNoRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorAssignsNoRole.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorBlock.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorBlock.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment4.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorWrongRoleNr.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorWrongRoleNr.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/ForLoop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/ForLoop.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/IfCondition.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/IfCondition.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/MainMethodCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/MainMethodCall.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/NewNonRoleObject.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/NewNonRoleObject.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/NewRoleObject.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/NewRoleObject.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/NonRoleMethodCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/NonRoleMethodCall.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/PureMethodCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/PureMethodCall.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/RoleFieldAssignment.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/RoleFieldAssignment.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WaitStatement.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WaitStatement.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WhileCondition.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WhileCondition.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongSyntax.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongSyntax.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion4.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/ForLoop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/ForLoop.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/LoopStatementInRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/LoopStatementInRole.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTerminationNonMain/MainCallFromRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/MainCallFromRole.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/WaitStatementInRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/WaitStatementInRole.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassFieldType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassFieldType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassFieldType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassFieldType2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassMethodType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassMethodType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassMethodType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassMethodType2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType4.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTypesNonMain/RoleMethodType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleMethodType.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleMethodType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleMethodType2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleMethodType3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleMethodType3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/VoidField.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/VoidField.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
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

  it should "pass with silicon and examples/witnesses/WandDemoSilver.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/WandDemoSilver.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
