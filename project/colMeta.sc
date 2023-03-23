import $file.common

import mill._
import scalalib._
import modules.Jvm

import common.{Dir, VercorsModule, ScalaPBModule}

object meta extends VercorsModule {
  def key = "colhelper"
  def deps = Agg(
    ivy"org.scalameta::scalameta:4.4.9",
    ivy"com.google.protobuf:protobuf-java:3.19.6",
  )

  def nodeDefinitions = T.sources { Dir.src / "col" / "vct" / "col" / "ast" / "Node.scala" }

  def helperSources = T {
    Jvm.runSubprocess(
      mainClass = "ColHelper",
      classPath = runClasspath().map(_.path),
      mainArgs = Seq(
        nodeDefinitions().map(_.path.toString).mkString(":"),
        T.dest.toString
      ),
    )

    PathRef(T.dest)
  }

  def helpers = T.sources { helperSources().path / "java" }
  def protobuf = T.sources { helperSources().path / "protobuf" }
}

object proto extends ScalaPBModule {
  def scalaPBSources = meta.protobuf
  def scalaPBFlatPackage = true
}