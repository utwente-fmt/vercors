package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class VeyMontToolPaperSpec extends VercorsSpec {
  val wd = "publications/2023/VeyMontToolPaper"
  val applicability = s"$wd/applicability"
  val paperExamples = s"$wd/paper-examples"

  vercors should verify using silicon flag
    "--veymont-generate-permissions" example s"$applicability/2pc-3.pvl"
  vercors should verify using silicon flag
    "--veymont-generate-permissions" example s"$applicability/2pc-5.pvl"
  vercors should verify using silicon flag
    "--veymont-generate-permissions" example s"$applicability/consensus-3.pvl"
  vercors should verify using silicon flag
    "--veymont-generate-permissions" example s"$applicability/consensus-5.pvl"
  vercors should verify using silicon flag
    "--veymont-generate-permissions" example s"$applicability/election-3.pvl"
  vercors should verify using silicon flag
    "--veymont-generate-permissions" example s"$applicability/election-5.pvl"

  // vercors should verify using silicon flag "--veymont-generate-permissions" example s"$paperExamples/veymont-swap.pvl"

  // Slow because of generated permisissions. Can fix when VeyMont has permission support.
//  (vercors
//    should verify
//    using silicon
//    flags("--veymont-generate-permissions", "--backend-option", "--assumeInjectivityOnInhale", "--dev-assert-timeout", "300")
//    example s"$paperExamples/veymont-tictactoemn.pvl"
//    )
  // (vercors
  //   should verify
  //   using silicon
  //   flags("--veymont-generate-permissions", "--backend-option", "--assumeInjectivityOnInhale", "--dev-assert-timeout", "300")
  //   example s"$paperExamples/veymont-tictactoemn-seq.pvl"
  //   )
}
