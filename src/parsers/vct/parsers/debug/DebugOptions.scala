package vct.parsers.debug

object DebugOptions {
  def NONE: DebugOptions = DebugOptions(reportAmbiguities = false, reportContextSensitivity = false)
  def ALL: DebugOptions = DebugOptions(reportAmbiguities = true, reportContextSensitivity = true)
}

case class DebugOptions(
  reportAmbiguities: Boolean,
  reportContextSensitivity: Boolean,
) {
  def fullContextEnabled: Boolean = reportContextSensitivity || reportAmbiguities
}
