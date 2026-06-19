package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class SYCLFullProgramsSpec extends VercorsSpec {
  vercors should verify using silicon flag
    "--no-infer-heap-context-into-frame" example
    "concepts/sycl/fullExamples/VectorAdd.cpp"

  vercors should verify using silicon flags("--no-infer-heap-context-into-frame",
    "--prover-config=smt.arith.solver=6",
    "--backend-option", "--numberOfErrorsToReport=0",
    "--backend-option","--moreJoins=2") example
    "concepts/sycl/fullExamples/shiftFunctions/ShiftGroupLeft.cpp"

  // The example verifies, but gets stuck once in a while so I'm disabling it. The same for the Helper file.
//  vercors should verify using silicon flags("--no-infer-heap-context-into-frame",
//    "--prover-config=smt.arith.solver=6",
//    "--backend-option", "--numberOfErrorsToReport=0",
//    "--backend-option", "--moreJoins=2") example
//    "concepts/sycl/kernels/ShiftGroupSum.cpp"


   vercors should verify using silicon flags("--no-infer-heap-context-into-frame",
    "--prover-config=smt.arith.solver=6",
    "--backend-option", "--numberOfErrorsToReport=0",
    "--backend-option", "--moreJoins=2") example
     "concepts/sycl/fullExamples/MatrixTransposeWithF.cpp"
}
