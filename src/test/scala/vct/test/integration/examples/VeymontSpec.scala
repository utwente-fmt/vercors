package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class VeymontSpec extends VercorsSpec {
  // https://github.com/utwente-fmt/vercors/issues/840
  // vercors should verify using silicon examples("concepts/veymont/IntGrid.pvl", "concepts/veymont/Move.pvl", "concepts/veymont/TicTacToe.pvl")
  // vercors should verify using silicon example "concepts/veymont/leaderelectring.pvl"
  // vercors should verify using silicon example "concepts/veymont/leaderelectstar.pvl"
  // vercors should verify using silicon example "concepts/veymont/paperscissorsrock.pvl"
  // vercors should verify using silicon example "concepts/veymont/parallel_while.pvl"
  // vercors should verify using silicon example "concepts/veymont/parallel_while_simplified.pvl"
  // vercors should verify using silicon example "concepts/veymont/binary-trees-10.pvl"
  // vercors should verify using silicon example "concepts/veymont/binary-trees-12.pvl"
  // vercors should verify using silicon example "concepts/veymont/binary-trees-2.pvl"
  // vercors should verify using silicon example "concepts/veymont/binary-trees-4.pvl"
  // vercors should verify using silicon example "concepts/veymont/binary-trees-6.pvl"
  // vercors should verify using silicon example "concepts/veymont/binary-trees-8.pvl"
  // vercors should verify using silicon example "concepts/veymont/binary-trees.pvl"
  // vercors should verify using silicon example "concepts/veymont/k-nucleotide-1.pvl"
  // vercors should verify using silicon example "concepts/veymont/k-nucleotide-2.pvl"
  // vercors should verify using silicon example "concepts/veymont/k-nucleotide-3.pvl"
  // vercors should verify using silicon example "concepts/veymont/k-nucleotide-4.pvl"
  // vercors should verify using silicon example "concepts/veymont/spectral-norm.pvl"
  vercors should verify using silicon example "concepts/veymont/leaderelectring-with-permissions.pvl"
  vercors should verify using silicon example "concepts/veymont/leaderelectstar-with-permissions.pvl"
}
