package viper.api

import viper.silver.reporter._

object NopViperReporter extends Reporter {
  override val name: String = "nopReporter"
  override def report(msg: Message): Unit = {}
}
