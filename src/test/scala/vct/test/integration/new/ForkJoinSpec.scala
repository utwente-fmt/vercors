package integration.`new`

import vct.test.integration.helper.VercorsSpec

class ForkJoinSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/forkjoin/forkfail.pvl"
  vercors should verify using anyBackend example "concepts/forkjoin/forkjoininforloop.pvl"
  vercors should verify using anyBackend example "concepts/forkjoin/forkpass.pvl"
  vercors should verify using anyBackend example "concepts/forkjoin/update-point.pvl"
  vercors should verify using anyBackend example "concepts/forkjoin/fibonacci.pvl"
  vercors should verify using anyBackend examples("concepts/forkjoin/OwickiGries.pvl", "concepts/forkjoin/Worker.pvl")
  vercors should error withCode "?" example "concepts/forkjoin/TestFork.pvl"
}
