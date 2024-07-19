package hre.log

import ch.qos.logback.classic.Level

sealed trait Verbosity {
  def asLogback: Level =
    this match {
      case Verbosity.Off => Level.OFF
      case Verbosity.Error => Level.ERROR
      case Verbosity.Warning => Level.WARN
      case Verbosity.Info => Level.INFO
      case Verbosity.Debug => Level.DEBUG
      case Verbosity.Trace => Level.TRACE
      case Verbosity.All => Level.ALL
    }
}

case object Verbosity {
  case object Off extends Verbosity
  case object Error extends Verbosity
  case object Warning extends Verbosity
  case object Info extends Verbosity
  case object Debug extends Verbosity
  case object Trace extends Verbosity
  case object All extends Verbosity
}
