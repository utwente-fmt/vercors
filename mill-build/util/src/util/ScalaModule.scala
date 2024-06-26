package util

import mill.T
import mill.scalalib.{ScalaModule => BaseScalaModule}

trait ScalaModule extends BaseScalaModule with JavaModule {
  def scalaVersion = "3.4.2"

  override def scalacOptions = T {
    val shared = Seq(
      "-deprecation",
      "-feature",
    )

    if (strictOptions()) {
      Seq(
        "-Ypatmat-exhaust-depth", "off",
        "-Werror",
      ) ++ shared
    } else {
      Seq(
      ) ++ shared
    }
  }
}