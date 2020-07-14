package viper.api.util

import hre.config.{BooleanSetting, OptionParser}

object Configuration {
  val z3Progress = new BooleanSetting(false)

  def addOptions(parser: OptionParser): Unit = {
    parser.add(z3Progress.getEnable("Monitor progress of z3 (Unix only). Also causes --numberOfParallelVerifiers 1 in silicon, as well as trace=true and proof=true in z3."), "progress-z3")
  }
}
