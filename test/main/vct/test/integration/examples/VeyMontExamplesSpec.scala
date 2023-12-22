package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class VeyMontExamplesSpec extends VercorsSpec {
  val wd = "concepts/veymont"
  vercors should verify using silicon flags "--veymont-generate-permissions" examples(
    s"$wd/TicTacToe/Player.pvl",
    s"$wd/TicTacToe/Move.pvl",
    s"$wd/TicTacToe/TicTacToe.pvl",
    )
  vercors should verify using silicon flags "--veymont-generate-permissions" example s"$wd/leaderelectring.pvl"
  vercors should verify using silicon flags "--veymont-generate-permissions" example s"$wd/leaderelectstar.pvl"
  vercors should verify using silicon flags "--veymont-generate-permissions" example s"$wd/paperscissorsrock.pvl"
  vercors should verify using silicon flags "--veymont-generate-permissions" example s"$wd/parallel_while.pvl"
  vercors should verify using silicon flags "--veymont-generate-permissions" example s"$wd/spectral-norm.pvl"
}
