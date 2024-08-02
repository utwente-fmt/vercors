package vct.test.integration.examples.veymont

import vct.test.integration.helper.VeyMontSpec

class FM2023VeyMontSpec extends VeyMontSpec {
  val wd = "publications/2023/FM2023VeyMont"
  val applicability = s"$wd/applicability"
  val paperExamples = s"$wd/paper-examples"
  val fs = Seq("--veymont-generate-permissions")

  // Verify with generated permissions
  choreography(inputs = examples(s"$applicability/2pc-3.pvl"), flags = fs)
  choreography(inputs = examples(s"$applicability/2pc-5.pvl"), flags = fs)
  choreography(inputs = examples(s"$applicability/consensus-3.pvl"), flags = fs)
  choreography(inputs = examples(s"$applicability/consensus-5.pvl"), flags = fs)
  choreography(inputs = examples(s"$applicability/election-3.pvl"), flags = fs)
  choreography(inputs = examples(s"$applicability/election-5.pvl"), flags = fs)

  // Verify without generated permissions
  choreography(inputs = examples(s"$paperExamples/veymont-swap.pvl"))

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
