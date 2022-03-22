package integration.`new`

import integration.helper.VercorsSpec

class AbruptExamplesSpec extends VercorsSpec {
  // Demos
  vercors should verify using silicon example "abrupt/Abrupt.java"
  vercors should verify using silicon example "abrupt/ExceptionsAndAbrupt.java"

  // Difference between carbon and silicon
  vercors should verify using silicon example "abrupt/KeYAbruptTerminationChallengeSilicon.java"
  vercors should verify using carbon example "abrupt/KeYAbruptTerminationChallengeCarbon.java"

  // Technical examples
  vercors should error withCode "?" example "abrupt/BadCustomException.java"
  vercors should error withCode "resolutionError" example "abrupt/CatchTypesIdenticalBad.java"
  vercors should error withCode "resolutionError" example "abrupt/CatchTypesSubtypeBad.java"
  vercors should verify using anyBackend example "abrupt/ContinueBreakFail.java"
  vercors should verify using anyBackend example "abrupt/ContinueBreakPass.java"
  vercors should verify using anyBackend example "abrupt/DoubleNestedFinally.java"
  vercors should verify using anyBackend example "abrupt/ExceptionAbortsAssignment.java"
  vercors should verify using anyBackend example "abrupt/Finally.java"
  vercors should verify using anyBackend example "abrupt/GoodCustomException.java"
  vercors should verify using anyBackend example "abrupt/KnuthTabulate.java"
  vercors should verify using anyBackend example "abrupt/LabeledStatements.java"
  vercors should verify using anyBackend example "abrupt/NestedTryCatchFinally.java"
  vercors should error withCode "?" example "abrupt/NoResultInSignals.java"
  vercors should verify using anyBackend example "abrupt/OnlyCatch.java"
  vercors should verify using anyBackend example "abrupt/OnlyReturn.java"
  vercors should verify using anyBackend example "abrupt/OnlyThrows.java"
  vercors should verify using anyBackend example "abrupt/OverloadedReturn.java"
  vercors should error withCode "?" example "abrupt/PureSignalsBad.java"
  vercors should error withCode "?" example "abrupt/PureThrowsBad.java"
  vercors should verify using anyBackend example "abrupt/ReturnFinally.java"
  vercors should error withCode "?" example "abrupt/SignalsMustExtendThrowable.java"
  vercors should verify using anyBackend example "abrupt/SignalsMustPropagate.java"
  vercors should verify using anyBackend example "abrupt/SignalsThrowNoThrows.java"
  vercors should verify using anyBackend example "abrupt/SwitchVarious.java"
  vercors should verify using anyBackend example "abrupt/Synchronized.java"
  vercors should error withCode "resolutionError" example "abrupt/ThrowIntBad.java"
  vercors should verify using anyBackend example "abrupt/ThrowsAndThrow.java"
  vercors should error withCode "resolutionError" example "abrupt/TypecheckSignalsBad.java"
  vercors should verify using anyBackend example "abrupt/TypecheckSignalsOk.java"
  vercors should error withCode "?" example "abrupt/TypecheckThrowsMethodBad.java"
  vercors should error withCode "?" example "abrupt/TypecheckThrowsThrowBad.java"
  vercors should verify using anyBackend example "abrupt/TypecheckTryCatch.java"
  vercors should error withCode "?" example "abrupt/UnusedCheckedCatchBad.java"
  vercors should verify using anyBackend example "abrupt/WhileBreakFinally.java"
  vercors should verify using anyBackend example "abrupt/WhileFinally.java"
  vercors should verify using anyBackend example "abrupt/WhileInFinally.java"
}
