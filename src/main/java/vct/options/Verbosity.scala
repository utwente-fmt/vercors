package vct.options

sealed trait Verbosity

case object Verbosity {
  case object Off extends Verbosity
  case object Error extends Verbosity
  case object Warning extends Verbosity
  case object Info extends Verbosity
  case object Debug extends Verbosity
  case object Trace extends Verbosity
  case object All extends Verbosity
}