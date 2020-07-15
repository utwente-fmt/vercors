lazy val hre = (project in file("."))
  .settings(
    name := "hre",
    version := "0.1-SNAPSHOT",

    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",

    // Disable documentation generation
    sources in (Compile, doc) := Seq(),
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false
  )