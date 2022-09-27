package vct.options

import hre.io.{Readable, Writeable}

import java.io.{FileReader, FileWriter, InputStreamReader, OutputStreamWriter, Reader, Writer}
import java.nio.charset.StandardCharsets

sealed trait PathOrStd extends Readable with Writeable {
  override def fileName: String = this match {
    case PathOrStd.Path(path) => path.toString
    case PathOrStd.StdInOrOut => "<stdio>"
  }

  override def isRereadable: Boolean = this match {
    case PathOrStd.Path(_) => true
    case PathOrStd.StdInOrOut => false
  }

  override protected def getReader: Reader = this match {
    case PathOrStd.Path(path) => new FileReader(path.toFile, StandardCharsets.UTF_8)
    case PathOrStd.StdInOrOut => new InputStreamReader(System.in, StandardCharsets.UTF_8)
  }

  override protected def getWriter: Writer = this match {
    case PathOrStd.Path(path) => new FileWriter(path.toFile, StandardCharsets.UTF_8)
    case PathOrStd.StdInOrOut => new OutputStreamWriter(System.out, StandardCharsets.UTF_8)
  }
}

case object PathOrStd {
  case class Path(path: java.nio.file.Path) extends PathOrStd
  case object StdInOrOut extends PathOrStd
}
