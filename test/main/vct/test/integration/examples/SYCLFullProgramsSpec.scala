package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class SYCLFullProgramsSpec extends VercorsSpec {
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/fullExamples/VectorAdd.cpp"

  // About 1 in 5 times this test will fail, because then VerCors fails to prove the pre-conditions of the second kernel even though they are true
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "concepts/sycl/fullExamples/MatrixTransposeWithF.cpp"
}