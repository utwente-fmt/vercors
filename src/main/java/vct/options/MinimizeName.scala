package vct.options

object MinimizeName {
  def parse(raw: String): MinimizeName = MinimizeName(raw.split('.'))
}

// Last one should always be a method or function (for now)
// Everything before that is either a package, for static/global decls, or package.class, for static decls
case class MinimizeName(path: Seq[String])

sealed abstract class MinimizeMode()
object MinimizeMode {
  final case object Focus extends MinimizeMode
  final case object Ignore extends MinimizeMode
}
