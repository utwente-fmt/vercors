package vct.options.types

trait ReadEnum[T] extends scopt.Read[T] {
  implicit def read: scopt.Read[T] = this

  def options: Map[String, T]

  override def arity: Int = 1
  override def reads: String => T = options(_)

  def valueName: String =
    options.keys.mkString("{", "|", "}")
}
