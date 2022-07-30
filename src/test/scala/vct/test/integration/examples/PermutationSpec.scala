package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class PermutationSpec extends VercorsSpec {

  vercors should verify using silicon example "publications/2020/permutations/bubble.pvl"
  vercors should verify using silicon example "publications/2020/permutations/evenPhase.pvl"
  vercors should verify using silicon example "publications/2020/permutations/insertion.pvl"
  vercors should verify using silicon example "publications/2020/permutations/merge1.pvl"
  vercors should verify using silicon example "publications/2020/permutations/merge2.pvl"
  vercors should verify using silicon example "publications/2020/permutations/oddEven.pvl"
  vercors should verify using silicon example "publications/2020/permutations/oddPhase.pvl"
  vercors should verify using silicon example "publications/2020/permutations/permutationProofLemmas.pvl"
  vercors should verify using silicon example "publications/2020/permutations/quick.pvl"
  vercors should verify using silicon example "publications/2020/permutations/selection.pvl"
  vercors should verify using silicon example "publications/2020/permutations/timsort1.pvl"
  vercors should verify using silicon example "publications/2020/permutations/timsort2.pvl"

}
