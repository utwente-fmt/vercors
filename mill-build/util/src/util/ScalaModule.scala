package util

import mill.T
import mill.scalalib.{ScalaModule => BaseScalaModule}

trait ScalaModule extends BaseScalaModule with JavaModule {
  def scalaVersion = "2.13.12"

  override def scalacOptions = T {
    val shared = Seq(
      "-deprecation",
      "-feature",
      "-Xno-patmat-analysis"
    )

    if (strictOptions()) {
      Seq(
        "-Ypatmat-exhaust-depth", "off",
        "-Werror",
      ) ++ shared
    } else {
      Seq(
        "-Xno-patmat-analysis",
      ) ++ shared
    }
  }
}