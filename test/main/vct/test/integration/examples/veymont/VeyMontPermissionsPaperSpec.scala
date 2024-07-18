package vct.test.integration.examples.veymont

import vct.test.integration.helper.VercorsSpec

class VeyMontPermissionsPaperSpec extends VercorsSpec {
  val wd = "concepts/veymont/FM2024 - VeyMont"

  vercors should verify using silicon flags
    "--veymont-generate-permissions" examples
    (s"$wd/0-TTT/Move.pvl", s"$wd/0-TTT/Player.pvl", s"$wd/0-TTT/0-TTT.pvl")

  vercors should verify using silicon flags "--dev-unsafe-optimization" examples
    (
      s"$wd/1-TTTmsg/Move.pvl",
      s"$wd/1-TTTmsg/Player.pvl",
      s"$wd/1-TTTmsg/1-TTTmsg.pvl",
    )

  vercors should verify using silicon flags "--dev-unsafe-optimization" examples
    (
      s"$wd/2-TTTlast/Move.pvl",
      s"$wd/2-TTTlast/Player.pvl",
      s"$wd/2-TTTlast/2-TTTlast.pvl",
    )
}
