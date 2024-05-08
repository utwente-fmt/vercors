package hre.resource

import hre.platform.Platform
import vct.result.VerificationError.SystemError

import java.net.URISyntaxException
import java.nio.file.{Path, Paths, Files}

case object ResourceUtil {
  case class NoSuchResource(path: String) extends SystemError {
    override def text: String = s"Could not find required resource at path $path"
  }

  val RESOURCE_ROOTS = Seq(Paths.get("res/universal/res"), Paths.get("res/universal/deps"))

  def getResource(path: String): Path = try {
    for(root <- RESOURCE_ROOTS) {
      if(Files.exists(root.resolve(path.substring(1)))) {
        return root.resolve(path.substring(1))
      }
    }
    
    throw NoSuchResource(path)
  } catch {
    case _: URISyntaxException => throw NoSuchResource(path)
  }

  def getPlatformDep(tail: String): Path = Platform.getCurrent match {
    case Platform.Windows => getResource(s"/win/$tail")
    case Platform.Unix => getResource(s"/unix/$tail")
    case Platform.Mac => getResource(s"/darwin/$tail")
  }

  def getPlatformBinary(tail: String): Path = Platform.getCurrent match {
    case Platform.Windows => getPlatformDep(tail + ".exe")
    case _ => getPlatformDep(tail)
  }
}
