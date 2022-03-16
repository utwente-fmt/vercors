package vct.resources

import hre.platform.Platform
import hre.resource.ResourceUtil.{getPlatformBinary, getResource}
import vct.result.VerificationResult.SystemError

import java.io.File
import java.net.URISyntaxException
import java.nio.file.{Path, Paths}

case object Resources {
  def getSimplificationPath(name: String): Path =
    getResource(s"/simplify/$name.pvl")

  def getAdtPath: Path = getResource("/adt")
  def getCIncludePath: Path = getResource("/c")
  def getJrePath: Path = getResource("/jdk")
  def getCcPath: Path = Paths.get("clang")
}
