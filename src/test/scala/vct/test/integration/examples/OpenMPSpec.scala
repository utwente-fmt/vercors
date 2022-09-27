package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class OpenMPSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/openmp/add-spec.c"
  vercors should verify using anyBackend example "concepts/openmp/add-spec-simd.c"
  vercors should verify using anyBackend example "concepts/openmp/addvec2.pvl"
  vercors should verify using anyBackend example "concepts/openmp/copy-spec.c"
  vercors should verify using anyBackend example "concepts/openmp/init-spec.c"
  vercors should verify using anyBackend example "concepts/openmp/sections-reduced.c"
  vercors should verify using anyBackend example "concepts/openmp/sections-reduced-fail.c"
  vercors should verify using anyBackend example "concepts/openmp/sum-spec.c"
  vercors should verify using anyBackend example "concepts/openmp/test-main.c"
  vercors should verify using anyBackend example "concepts/openmp/test-other.c"
  vercors should verify using anyBackend example "concepts/openmp/zero-spec.c"
}
