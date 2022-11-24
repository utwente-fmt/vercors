package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class AbruptExamplesSpec extends VercorsSpec {
  // Demos
  vercors should verify using silicon example "concepts/abrupt/Abrupt.java"
  vercors should verify using silicon example "concepts/abrupt/ExceptionsAndAbrupt.java"

  // Difference between carbon and silicon
  vercors should verify using silicon example "concepts/abrupt/KeYAbruptTerminationChallengeSilicon.java"

  // Technical examples
  vercors should error withCode "resolutionError" example "concepts/abrupt/CatchTypesIdenticalBad.java"
  vercors should error withCode "resolutionError" example "concepts/abrupt/CatchTypesSubtypeBad.java"
  vercors should verify using anyBackend example "concepts/abrupt/ContinueBreakFail.java"
  vercors should verify using anyBackend example "concepts/abrupt/ContinueBreakPass.java"
  vercors should verify using anyBackend example "concepts/abrupt/DoubleNestedFinally.java"
  vercors should verify using anyBackend example "concepts/abrupt/ExceptionAbortsAssignment.java"
  vercors should verify using anyBackend example "concepts/abrupt/Finally.java"
  vercors should verify using anyBackend example "concepts/abrupt/GoodCustomException.java"
  vercors should verify using anyBackend example "concepts/abrupt/KnuthTabulate.java"
  vercors should verify using anyBackend example "concepts/abrupt/LabeledStatements.java"
  vercors should verify using anyBackend example "concepts/abrupt/NestedTryCatchFinally.java"
  vercors should error withCode "resolutionError" example "concepts/abrupt/NoResultInSignals.java"
  vercors should verify using anyBackend example "concepts/abrupt/OnlyCatch.java"
  vercors should verify using anyBackend example "concepts/abrupt/OnlyReturn.java"
  vercors should verify using anyBackend example "concepts/abrupt/OnlyThrows.java"
  vercors should verify using anyBackend example "concepts/abrupt/OverloadedReturn.java"
  vercors should error withCode "notPure" example "concepts/abrupt/PureSignalsBad.java"
  vercors should error withCode "notPure" example "concepts/abrupt/PureThrowsBad.java"
  vercors should verify using anyBackend example "concepts/abrupt/ReturnFinally.java"
  vercors should verify using anyBackend example "concepts/abrupt/SignalsMustPropagate.java"
  vercors should verify using anyBackend example "concepts/abrupt/SignalsThrowNoThrows.java"
  vercors should verify using anyBackend example "concepts/abrupt/SwitchVarious.java"
  vercors should verify using anyBackend example "concepts/abrupt/Synchronized.java"
  vercors should error withCode "resolutionError" example "concepts/abrupt/ThrowIntBad.java"
  vercors should verify using anyBackend example "concepts/abrupt/ThrowsAndThrow.java"
  vercors should error withCode "resolutionError" example "concepts/abrupt/TypecheckSignalsBad.java"
  vercors should verify using anyBackend example "concepts/abrupt/TypecheckSignalsOk.java"
  vercors should verify using anyBackend example "concepts/abrupt/TypecheckTryCatch.java"
  vercors should verify using anyBackend example "concepts/abrupt/WhileBreakFinally.java"
  vercors should verify using anyBackend example "concepts/abrupt/WhileFinally.java"
  vercors should verify using anyBackend example "concepts/abrupt/WhileInFinally.java"
  vercors should verify using anyBackend example "concepts/abrupt/SynchronizedReturn.java"
  vercors should verify using carbon example "concepts/abrupt/KeYAbruptTerminationChallengeCarbon.java"

  // Examples that should be type-checked into an error (https://github.com/utwente-fmt/vercors/issues/790)
  // vercors should error withCode "?" example "concepts/abrupt/BadCustomException.java"
  // vercors should error withCode "?" example "concepts/abrupt/SignalsMustExtendThrowable.java"
  // vercors should error withCode "?" example "concepts/abrupt/TypecheckThrowsMethodBad.java"
  // vercors should error withCode "?" example "concepts/abrupt/TypecheckThrowsThrowBad.java"
  // vercors should error withCode "?" example "concepts/abrupt/UnusedCheckedCatchBad.java"

}
