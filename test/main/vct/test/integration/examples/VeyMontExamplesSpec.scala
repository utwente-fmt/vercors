package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class VeyMontExamplesSpec extends VercorsSpec {
  val wd = "concepts/veymont"
  vercors should verify using silicon examples(
    s"$wd/TicTacToe/IntGrid.pvl",
    s"$wd/TicTacToe/Move.pvl",
    s"$wd/TicTacToe/TicTacToe.pvl",
    )
  vercors should verify using silicon example s"$wd/leaderelectring.pvl"
  vercors should verify using silicon example s"$wd/leaderelectring-with-permissions.pvl" // TODO: Do we need to keep this?
  vercors should verify using silicon example s"$wd/leaderelectstar.pvl"
  vercors should verify using silicon example s"$wd/leaderelectstar-with-permissions.pvl" // TODO: Do we need to keep this?
  vercors should verify using silicon example s"$wd/paperscissorsrock.pvl"
  vercors should verify using silicon example s"$wd/parallel_while.pvl"
  vercors should verify using silicon example s"$wd/parallel_while_simplified.pvl"
  vercors should verify using silicon example s"$wd/spectral-norm.pvl"
}
