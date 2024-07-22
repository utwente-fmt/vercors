package vct.test.integration.examples.veymont

import vct.test.integration.helper.VercorsSpec

class FM2023VeyMontSpec extends VercorsSpec {
  val wd = "publications/2023/FM2023VeyMont"
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

  vercors should verify using silicon example s"$paperExamples/veymont-swap.pvl"

  // Slow because of generated permisissions. Can fix when VeyMont has permission support. For now in known-problems
  // Also: needs sub runs, otherwise the program blows up quite a bit.
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
