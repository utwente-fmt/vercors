package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class BasicExamplesSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/basic/AddAssignJava.java"
  vercors should error withCode "resolutionError:type" example "concepts/basic/ArrayAsObject.java"
  vercors should verify using anyBackend example "concepts/basic/BasicAssert.java"
  vercors should verify using anyBackend example "concepts/basic/BasicAssert-e1.java"
  vercors should verify using anyBackend example "concepts/basic/BooleanOperators.java"
  vercors should error withCode "resolutionError:type" example "concepts/basic/Boxing.java"
  vercors should verify using anyBackend example "concepts/basic/CollectionTest.pvl"
  vercors should verify using anyBackend example "concepts/basic/ContractUnsatisfiableIntentional.java"
  vercors should verify using anyBackend example "concepts/basic/ContractUnsatisfiableIntentional.pvl"
  vercors should verify using anyBackend example "concepts/basic/ContractUnsatisfiableUnintentional.java"
  vercors should verify using anyBackend example "concepts/basic/ContractUnsatisfiableUnintentional.pvl"
  vercors should verify using anyBackend example "concepts/basic/Sat.java"
  vercors should verify using anyBackend example "concepts/basic/Unsat.java"
  vercors should verify using anyBackend example "concepts/basic/For.java"
  vercors should verify using anyBackend example "concepts/basic/for.pvl"
  vercors should verify using anyBackend example "concepts/basic/frac1.pvl"
  vercors should verify using anyBackend example "concepts/basic/frac2.pvl"
  vercors should verify using anyBackend example "concepts/basic/fraction-comparison.pvl"
  vercors should verify using anyBackend example "concepts/basic/InlineFunctions.pvl"
  vercors should verify using silicon example "concepts/basic/MultiDimArray.java"
  vercors should verify using anyBackend example "concepts/basic/NewClassGhost.java"
  // https://github.com/utwente-fmt/vercors/issues/921
  // vercors should verify using anyBackend example "concepts/basic/pointer.c"
  vercors should verify using anyBackend example "concepts/basic/postfix-increment.pvl"
  vercors should verify using anyBackend example "concepts/basic/predicate.pvl"
  vercors should error withCode "curThreadScope" example "concepts/basic/pure.pvl"
  vercors should verify using anyBackend example "concepts/basic/pvl-array.pvl"
  vercors should error withCode "notALocation" example "concepts/basic/seq-immutable.pvl"
  vercors should verify using anyBackend example "concepts/basic/seq-item-access.pvl"
  vercors should verify using anyBackend example "concepts/basic/seq-length.pvl"
  vercors should verify using anyBackend example "concepts/basic/seq-seq-length.pvl"
  vercors should verify using anyBackend example "concepts/basic/sumints.pvl"
  vercors should verify using anyBackend example "concepts/basic/TernaryOperator.java"
  vercors should verify using anyBackend example "concepts/basic/test-1.c"
  // https://github.com/utwente-fmt/vercors/issues/769
  // vercors should verify using anyBackend example "concepts/basic/test-scale.java"
  // PB TODO: add equivalent of --dev-no-sat
  // vercors should verify using anyBackend example "concepts/basic/TurnOffContractSatisfiable.pvl"
  vercors should verify using anyBackend example "concepts/basic/yield.pvl"
  vercors should verify using anyBackend example "concepts/basic/DafnyIncr.java"
  vercors should verify using silicon example "concepts/basic/DafnyIncrE1.java"
  vercors should failVerification withCode "perm" using carbon example "concepts/basic/DafnyIncrE1.java"
  vercors should error withCode "resolutionError:type" example "concepts/basic/BadType.pvl"
  vercors should error withCode "notALocation" example "concepts/basic/BadType.java"
  vercors should verify using anyBackend example "concepts/basic/functions.pvl"
  vercors should verify using anyBackend example "concepts/basic/induction-problem.pvl"
  vercors should verify using anyBackend example "concepts/basic/induction-lemma.pvl"
  vercors should verify using anyBackend example "concepts/basic/option.pvl"
  vercors should verify using anyBackend example "concepts/basic/BoogieExampleFail.java"
  vercors should verify using anyBackend example "concepts/basic/BoogieTestFail.java"
  vercors should verify using anyBackend example "concepts/basic/BoogieExamplePass.java"
  vercors should verify using anyBackend example "concepts/basic/BoogieTest.java"
  vercors should verify using anyBackend example "concepts/basic/LoopInvFail.java"
  vercors should verify using anyBackend example "concepts/basic/LoopInvPass.java"
  vercors should verify using anyBackend example "concepts/basic/SimpleExamples.java"
  vercors should verify using anyBackend example "concepts/basic/nested-loops.pvl"
  vercors should verify using anyBackend example "concepts/basic/BigInts.pvl"
  vercors should verify using anyBackend example "concepts/basic/RequireConstructor.pvl"
  // https://github.com/utwente-fmt/vercors/issues/781
  // vercors should verify using anyBackend example "concepts/basic/StaticImportA.java"
  // vercors should verify using anyBackend example "concepts/basic/StaticImportB.java"
  vercors should verify using anyBackend example "concepts/basic/par-pointer-simplification.c"
}
