package integration.`new`

import vct.test.integration.helper.VercorsSpec

class LoopDependencySpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/loopdeps/backward-dep-e1.c"
  vercors should verify using anyBackend example "concepts/loopdeps/forward-dep.pvl"
  vercors should verify using anyBackend example "concepts/loopdeps/forward-dep-e1.c"
  vercors should verify using anyBackend example "concepts/loopdeps/forward-dep-noauto.pvl"
  vercors should verify using anyBackend example "concepts/loopdeps/forward-loop.c"
}
