package vct.main.passes

import vct.logging.PassReport
import vct.main.passes.Passes.BY_KEY

class VeymontPassesGenerator extends PassesGeneratorTrait {
  def getPasses(report: PassReport): Seq[AbstractPass] = Seq(
    BY_KEY("VeyMontStructCheck"),
    BY_KEY("VeyMontTerminationCheck"),
    //  BY_KEY("VeyMontGlobalLTS"),
    BY_KEY("VeyMontDecompose"),
    BY_KEY("VeyMontLocalLTS"),
    BY_KEY("removeTaus"),
    BY_KEY("removeEmptyBlocks"),
    BY_KEY("VeyMontBarrier"),
    BY_KEY("VeyMontLocalProgConstr"),
    BY_KEY("VeyMontAddChannelPerms"),
    BY_KEY("VeyMontAddStartThreads"),
    BY_KEY("printPVL"),
  )
}
