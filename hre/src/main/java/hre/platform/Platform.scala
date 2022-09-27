package hre.platform

import vct.result.VerificationError.UserError

sealed trait Platform

case object Platform {
  case object Windows extends Platform
  case object Unix extends Platform
  case object Mac extends Platform

  case class UnknownPlatform(name: String) extends UserError {
    override def text: String = s"Unknown platform: $name. Please specify paths to binaries for z3 and boogie."
    override def code: String = "unknownPlatform"
  }

  def getCurrent: Platform = {
    val name = System.getProperty("os.name").toLowerCase
    if(name.contains("win")) Platform.Windows
    else if(name.contains("linux")) Platform.Unix
    else if(name.contains("nix")) Platform.Unix
    else if(name.contains("mac")) Platform.Mac
    else throw UnknownPlatform(name)
  }
}
