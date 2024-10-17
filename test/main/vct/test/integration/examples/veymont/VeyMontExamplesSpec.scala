package vct.test.integration.examples.veymont

import vct.test.integration.helper.VeyMontSpec

class VeyMontExamplesSpec extends VeyMontSpec {
  {
    val wd = "concepts/veymont/generatedPermissions"

    choreography(
      desc = "Tic-Tac-Toe (generated permissions)",
      inputs = examples(
        s"$wd/TicTacToe/Player.pvl",
        s"$wd/TicTacToe/Move.pvl",
        s"$wd/TicTacToe/TicTacToe.pvl",
      ),
      flags = Seq("--generate-permissions", "--veymont-ps=inline"),
    )

    choreography(
      desc = "Leader elect ring (generated permissions)",
      inputs = examples(s"$wd/leaderelectring.pvl"),
      flags = Seq("--generate-permissions", "--veymont-ps=inline"),
    )

    choreography(
      desc = "Leader elect star (generated permissions)",
      inputs = examples(s"$wd/leaderelectstar.pvl"),
      flags = Seq("--generate-permissions"),
    )

    choreography(
      desc = "Paper-scissors-rock (generated permissions)",
      inputs = examples(s"$wd/paperscissorsrock.pvl"),
      flags = Seq("--generate-permissions", "--veymont-ps=inline"),
    )

    choreography(
      desc = "Parallel while (generated permissions)",
      inputs = examples(s"$wd/parallel_while.pvl"),
      flags = Seq("--generate-permissions"),
    )

    // Disabled indefinitely until submethods are enabled again
    // vercors should verify using silicon flags
    //   "--generate-permissions" example s"$wd/spectral_norm.pvl"
  }

  {
    val wd = "concepts/veymont/annotatedPermissions"

    choreography(
      desc = "Swap (annotated permissions)",
      inputs = examples(s"$wd/swap.pvl"),
    )
  }
}
