package vct.resources

import hre.platform.Platform
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
    getResource(s"/simplify/$name.pvl")

  def getAdtPath: Path = getResource("/adt")
  def getCIncludePath: Path = getResource("/c")
  def getJrePath: Path = getResource("/jdk")
  def getCcPath: Path = Paths.get("clang")

  def getPlatformDep(tail: String): Path = Platform.getCurrent match {
    case Platform.Windows => getResource(s"/win/$tail")
    case Platform.Unix => getResource(s"/unix/$tail")
    case Platform.Mac => getResource(s"/darwin/$tail")
  }

  def getPlatformBinary(tail: String): Path = Platform.getCurrent match {
    case Platform.Windows => getPlatformDep(tail + ".exe")
    case _ => getPlatformDep(tail)
  }

  def getZ3Path: Path = getPlatformBinary("z3/bin/z3")
  def getBoogiePath: Path = getPlatformBinary("boogie/Boogie")
}
