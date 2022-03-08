package vct.options

trait PathOrStd

case object PathOrStd {
  case class Path(path: java.nio.file.Path) extends PathOrStd
  case object StdInOrOut extends PathOrStd
}
