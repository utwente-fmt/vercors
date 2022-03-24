package integration.`new`

import integration.helper.VercorsSpec

class VeymontSpec extends VercorsSpec {
  vercors should verify using silicon examples("concepts/veymont/IntGrid.pvl", "concepts/veymont/Move.pvl", "concepts/veymont/TicTacToe.pvl")
  vercors should verify using silicon example "concepts/veymont/leaderelectring.pvl"
  vercors should verify using silicon example "concepts/veymont/leaderelectstar.pvl"
  vercors should verify using silicon example "concepts/veymont/paperscissorsrock.pvl"
  vercors should verify using silicon example "concepts/veymont/parallel_while.pvl"
  vercors should verify using silicon example "concepts/veymont/parallel_while_simplified.pvl"
}
