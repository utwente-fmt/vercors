lazy val hre = (project in file(".")).settings(
  name := "hre",
  version := "0.1-SNAPSHOT",

  libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
  libraryDependencies += "org.fusesource.jansi" % "jansi" % "2.4.0",
  libraryDependencies += "net.harawata" % "appdirs" % "1.2.1",
  libraryDependencies += "net.java.dev.jna" % "jna" % "5.13.0",

  // Disable documentation generation
  Compile / doc / sources := Nil,
  Compile / packageDoc / publishArtifact := false,

  Compile / PB.targets := Seq(
    scalapb.gen(flatPackage = true) -> (Compile / sourceManaged).value / "scalapb"
  ),

  Compile / PB.protoSources ++= Seq(
    (Compile / sourceManaged).value / "protobuf"
  ),
)