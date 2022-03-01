package vct.resources

import vct.result.VerificationResult.SystemError

import java.io.File
import java.net.URISyntaxException
import java.nio.file.{Path, Paths}

case object Resources {
  case class NoSuchResource(path: String) extends SystemError {
    override def text: String = s"Could not find required resource at path $path"
  }

  def getResource(path: String): Path = try {
    Option(getClass.getResource(path)) match {
      case Some(url) => new File(url.toURI).toPath
      case None => throw NoSuchResource(path)
    }
  } catch {
    case _: URISyntaxException => throw NoSuchResource(path)
  }

  def getSimplificationPath(name: String): Path =
    getResource(s"/config/$name.pvl")

  def getAdtPath: Path = getResource("/adt")
  def getCIncludePath: Path = getResource("/include")
  def getJrePath: Path = getResource("/jdk")
  def getZ3Path: Path = getResource("/jdk")
  def getBoogiePath: Path = getResource("/jdk")
  def getCcPath: Path = Paths.get("clang")
}
