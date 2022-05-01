package vct.options

object MinimizeTarget {
  def parse(raw: String): Option[MinimizeTarget] = {
    raw.split(",", 2).toSeq match {
      case Seq(fqn, methodName) => Some(MinimizeTarget(fqn.split("."), methodName))
      case _ => None
    }
  }
}

case class MinimizeTarget(`class`: Seq[String], name: String)

sealed abstract class MinimizeMode()
object MinimizeMode {
  final case object Focus extends MinimizeMode
  final case object Ignore extends MinimizeMode
}
