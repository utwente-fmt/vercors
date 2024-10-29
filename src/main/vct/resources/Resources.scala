package vct.resources

import hre.resource.ResourceUtil.getResource

import java.nio.file.{Path, Paths}

case object Resources {
  def getSimplificationPath(name: String): Path =
    getResource(s"/simplify/$name.pvl")

  def getAdtPath: Path = getResource("/adt")
  def getCIncludePath: Path = getResource("/c")
  def getCPPIncludePath: Path = getResource("/cpp")
  def getJrePath: Path = getResource("/jdk")
  def getCcPath: Path = Paths.get("clang")
  def getCPPcPath: Path = Paths.get("clang++")
  def getSystemCConfig: Path = getResource("/systemc/config")
  def getVeymontPath: Path = getResource("/veymont")
  def getPallas: Path = getResource("/pallas")
}
