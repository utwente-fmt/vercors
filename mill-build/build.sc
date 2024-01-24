import mill._
import mill.util.Jvm
import scalalib._

object millbuild extends MillBuildRootModule {
  def moduleDeps = Seq(structure, util)

  def nodeSources = T.sources(
    T.ctx().workspace / "src" / "col" / "vct" / "col" / "ast" / "Node.scala"
  )

  def analyseNodeDeclarations = T {
    Jvm.runSubprocess(
      mainClass = "vct.col.ast.analysis.Analysis",
      classPath = analysis.runClasspath().map(_.path),
      mainArgs = T.dest.toString() +: nodeSources().map(_.path.toString()),
    )

    ()
  }

  def structureClassPath = T {
    os.write(T.dest / "classpath.json", upickle.default.write(structure.runClasspath().map(_.path)))
    ()
  }

  def resources = T {
    analyseNodeDeclarations()
    structureClassPath()
    Seq.empty[PathRef]
  }

  def compileResources = T { Seq.empty[PathRef] }

  object util extends ScalaModule {
    def scalaVersion = "2.13.12"
    def ivyDeps = T {
      Seq(
        ivy"com.lihaoyi::mill-main:0.11.0",
        ivy"com.lihaoyi::mill-main-api:0.11.0",
        ivy"com.lihaoyi::mill-scalalib:0.11.0",
        ivy"com.lihaoyi::mill-contrib-scalapblib:0.11.0",
        ivy"me.pieterbos::mill-cpp_mill0.11::0.0.1",
      )
    }
    def compileResources = T { Seq.empty[PathRef] }
    def resources = T { Seq.empty[PathRef] }
  }

  object structure extends ScalaModule {
    def scalaVersion = "2.13.12"
    def ivyDeps = T { Seq(ivy"com.lihaoyi::upickle:3.1.3", ivy"com.lihaoyi::os-lib:0.9.3") }
    def compileResources = T { Seq.empty[PathRef] }
    def resources = T { Seq.empty[PathRef] }
  }

  object analysis extends ScalaModule {
    def scalaVersion = "2.13.12"
    def moduleDeps = Seq(structure)
    def ivyDeps = T { Seq(ivy"org.scalameta::scalameta:4.8.15", ivy"com.lihaoyi::os-lib:0.9.3") }
    def compileResources = T { Seq.empty[PathRef] }
    def resources = T { Seq.empty[PathRef] }
  }
}