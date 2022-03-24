package integration.`new`

import vct.test.integration.helper.VercorsSpec

class CounterSpec extends VercorsSpec {
  vercors should verify using anyBackend examples("concepts/counter/permissions.pvl", "concepts/counter/loop.pvl")
  vercors should verify using anyBackend examples("concepts/counter/permissions.pvl", "concepts/counter/main.pvl")
  vercors should verify using anyBackend examples("concepts/counter/permissions.pvl", "concepts/counter/parameters1.pvl")
  vercors should verify using anyBackend examples("concepts/counter/permissions.pvl", "concepts/counter/parameters2.pvl")
}
