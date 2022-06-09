package vct.options

sealed abstract class MinimizeMode()
object MinimizeMode {
  final case object Focus extends MinimizeMode
  final case object Ignore extends MinimizeMode
}
