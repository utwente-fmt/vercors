package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class PermutationSpec extends VercorsSpec {
  vercors should verify using silicon example "publications/2020/permutations/permutation.pvl"
  vercors should verify using silicon examples("publications/2020/permutations/evenPhase.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/oddEven.pvl", "publications/2020/permutations/permutation.pvl")
  // vercors should verify using silicon examples("publications/2020/permutations/oddPhase.pvl", "publications/2020/permutations/permutation.pvl)
  vercors should verify using silicon examples("publications/2020/permutations/bubble.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/insertion.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/merge1.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/merge2.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/quick.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/selection.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/timsort1.pvl", "publications/2020/permutations/permutation.pvl")
  vercors should verify using silicon examples("publications/2020/permutations/timsort2.pvl", "publications/2020/permutations/permutation.pvl")
}
