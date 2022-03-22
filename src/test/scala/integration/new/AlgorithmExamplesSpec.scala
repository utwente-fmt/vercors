package integration.`new`

import integration.helper.VercorsSpec

class AlgorithmExamplesSpec extends VercorsSpec {
  vercors should verify using silicon example "algo/BinarySearch.pvl"
  vercors should verify using silicon example "algo/DutchNationalFlag.pvl"
}
