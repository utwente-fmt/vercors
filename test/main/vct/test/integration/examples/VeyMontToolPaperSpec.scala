package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class VeyMontToolPaperSpec extends VercorsSpec {
  val wd = "publications/2023/VeyMontToolPaper"
  val applicability = s"$wd/applicability"
  val paperExamples = s"$wd/paper-examples"
  val performance = s"$wd/performance"

  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$applicability/2pc-3.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$applicability/2pc-5.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$applicability/consensus-3.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$applicability/consensus-5.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$applicability/election-3.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$applicability/election-5.pvl"

  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$paperExamples/veymont-swap.pvl"
//  vercors should verify using silicon example s"$paperExamples/veymont-tictactoemn.pvl"

  vercors should verify using silicon example s"$performance/binary-trees-2.pvl"
  vercors should verify using silicon example s"$performance/binary-trees-4.pvl"
  vercors should verify using silicon example s"$performance/binary-trees-6.pvl"
  vercors should verify using silicon example s"$performance/binary-trees-8.pvl"
  vercors should verify using silicon example s"$performance/k-nucleotide-1.pvl"
  vercors should verify using silicon example s"$performance/k-nucleotide-2.pvl"
  vercors should verify using silicon example s"$performance/k-nucleotide-3.pvl"
  vercors should verify using silicon example s"$performance/k-nucleotide-4.pvl"
}
