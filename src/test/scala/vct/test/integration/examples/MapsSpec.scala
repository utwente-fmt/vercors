package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class MapsSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/maps/maps.pvl"
  vercors should verify using silicon example "concepts/maps/maps_example_from_dafny.pvl"
}
