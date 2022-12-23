package vct.options.types

import java.nio.file.{Path, Paths}

sealed trait ClassPathEntry

case object ClassPathEntry extends scopt.Read[ClassPathEntry] {
  implicit def read: scopt.Read[ClassPathEntry] = this
  override def arity: Int = 1
  override def reads: String => ClassPathEntry = {
    case "@jre" => DefaultJre
    case "@source" => SourcePackageRoot
    case s => SourcePath(Paths.get(s))
  }

  case object DefaultJre extends ClassPathEntry
  case object SourcePackageRoot extends ClassPathEntry
  case class SourcePath(root: Path) extends ClassPathEntry
}