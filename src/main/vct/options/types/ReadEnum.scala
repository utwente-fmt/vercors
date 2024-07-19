package vct.options.types

import hre.log.Verbosity

object ReadEnum {
  implicit val readVerbosity: ReadEnum[Verbosity] =
    new ReadEnum[Verbosity] {
      import hre.log.Verbosity._

      override val options: Map[String, Verbosity] = Map(
        "off" -> Off,
        "error" -> Error,
        "warning" -> Warning,
        "info" -> Info,
        "debug" -> Debug,
        "trace" -> Trace,
        "all" -> All,
      )
    }
}

trait ReadEnum[T] extends scopt.Read[T] {
  implicit def read: scopt.Read[T] = this

  def options: Map[String, T]

  override def arity: Int = 1
  override def reads: String => T = options(_)

  def valueName: String = options.keys.mkString("{", "|", "}")
}
