package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class LoopDependencySpec extends VercorsSpec {
  // https://github.com/utwente-fmt/vercors/discussions/836
  vercors should verify using anyBackend example "concepts/loopdeps/backward-dep-e1.c"
  vercors should verify using silicon example "concepts/loopdeps/forward-dep-e1.c"
  vercors should verify using silicon example "concepts/loopdeps/forward-loop.c"
  vercors should verify using silicon example "concepts/loopdeps/forward-dep.pvl"
  vercors should verify using anyBackend example "concepts/loopdeps/forward-dep-noauto.pvl"
}
