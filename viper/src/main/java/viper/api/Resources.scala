package viper.api

import hre.resource.ResourceUtil.getPlatformBinary

import java.nio.file.Path

case object Resources {
  def getZ3Path: Path = getPlatformBinary("z3/bin/z3")
  def getBoogiePath: Path = getPlatformBinary("boogie/Boogie")
}
