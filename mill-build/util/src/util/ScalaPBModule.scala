package util

import mill.contrib.scalapblib.{ScalaPBModule => BaseScalaPBModule}

trait ScalaPBModule extends BaseScalaPBModule with ScalaModule {
  def scalaPBVersion = "0.11.11"
  override def scalaPBFlatPackage = true
  override def scalaPBSearchDeps = true
}