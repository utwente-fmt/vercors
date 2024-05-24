package vct.options.types

import hre.io.{InMemoryCachedReadable, Watch, Writeable}

import java.io.{InputStreamReader, OutputStreamWriter, Reader, Writer}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

sealed trait PathOrStd extends InMemoryCachedReadable with Writeable {
  override def underlyingPath: Option[Path] = this match {
    case PathOrStd.Path(path) => Some(path)
    case PathOrStd.StdInOrOut => None
  }

  override def fileName: String = this match {
    case PathOrStd.Path(path) => path.toString
    case PathOrStd.StdInOrOut => "<stdio>"
  }

  override def isRereadable: Boolean = this match {
    case PathOrStd.Path(_) => true
    case PathOrStd.StdInOrOut => false
  }

  override protected def getReaderImpl: Reader = this match {
    case PathOrStd.Path(path) => Files.newBufferedReader(path, StandardCharsets.UTF_8)
    case PathOrStd.StdInOrOut => new InputStreamReader(System.in, StandardCharsets.UTF_8)
  }

  override protected def getWriter: Writer = {
    invalidate()
    this match {
      case PathOrStd.Path(path) => Files.newBufferedWriter(path, StandardCharsets.UTF_8)
      case PathOrStd.StdInOrOut => new OutputStreamWriter(System.out, StandardCharsets.UTF_8)
    }
  }

  override def enroll(watch: Watch): Unit = this match {
    case PathOrStd.Path(path) =>
      watch.enroll(path)
      watch.invalidate(this)
    case PathOrStd.StdInOrOut => // do nothing
  }
}

case object PathOrStd {
  case class Path(path: java.nio.file.Path) extends PathOrStd
  case object StdInOrOut extends PathOrStd
}
