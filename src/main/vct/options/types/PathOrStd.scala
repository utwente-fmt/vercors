package vct.options.types

import hre.io.{InMemoryCachedReadable, Writeable}

import java.io._
import java.nio.charset.StandardCharsets

sealed trait PathOrStd extends InMemoryCachedReadable with Writeable {
  override def underlyingFile: Option[File] = this match {
    case PathOrStd.Path(path) => Some(path.toFile)
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
    case PathOrStd.Path(path) => new FileReader(path.toFile, StandardCharsets.UTF_8)
    case PathOrStd.StdInOrOut => new InputStreamReader(System.in, StandardCharsets.UTF_8)
  }

  override protected def getWriter: Writer = {
    invalidate()
    this match {
      case PathOrStd.Path(path) => new FileWriter(path.toFile, StandardCharsets.UTF_8)
      case PathOrStd.StdInOrOut => new OutputStreamWriter(System.out, StandardCharsets.UTF_8)
    }
  }
}

case object PathOrStd {
  case class Path(path: java.nio.file.Path) extends PathOrStd {
    def isDir: Boolean = underlyingFile.exists(_.isDirectory)
    def mkDir() = {
      assert(isDir)
      val dir = underlyingFile.get
      dir.mkdirs()
    }

    def resolve(elem: String): Path = Path(path.resolve(elem))
  }
  case object StdInOrOut extends PathOrStd
}
