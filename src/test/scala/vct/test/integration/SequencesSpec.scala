package vct.test.integration

import vct.test.integration.helper.VercorsSpec

class SequencesSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/sequences/append-prepend-sequence.pvl"
  vercors should verify using silicon example "concepts/sequences/bubble-sort.pvl"
  vercors should verify using silicon example "concepts/sequences/max-sort.pvl"
  vercors should verify using silicon example "concepts/sequences/remove-value.pvl"
  vercors should verify using silicon example "concepts/sequences/sequence-constructors.pvl"
  vercors should verify using silicon example "concepts/sequences/slicing.pvl"
  vercors should verify using silicon example "concepts/sequences/updating.pvl"
}
