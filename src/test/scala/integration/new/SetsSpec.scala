package integration.`new`

import integration.helper.VercorsSpec

class SetsSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/sets/subset.pvl"
  vercors should verify using silicon example "concepts/sets/set_comprehension.pvl"
}
