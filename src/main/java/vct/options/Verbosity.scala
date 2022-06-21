package vct.options

sealed trait Verbosity

case object Verbosity extends ReadEnum[Verbosity] {
  override val options: Map[String, Verbosity] = Map(
    "off" -> Off,
    "error" -> Error,
    "warning" -> Warning,
    "info" -> Info,
    "debug" -> Debug,
    "trace" -> Trace,
    "all" -> All,
  )

  case object Off extends Verbosity
  case object Error extends Verbosity
  case object Warning extends Verbosity
  case object Info extends Verbosity
  case object Debug extends Verbosity
  case object Trace extends Verbosity
  case object All extends Verbosity
}