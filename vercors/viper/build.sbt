import java.nio.file.{Files, Path, Paths}
import java.net.URL
import java.util.Comparator
import sbt.internal._

/* To update viper, replace the hash with the commit hash that you want to point to. It's a good idea to ask people to
 re-import the project into their IDE, as the location of the viper projects below will change. */
val silver_url = uri("hg:https://bitbucket.org/viperproject/silver#1a2059df2fc348a6a777e73e00ad10a4c129da0f")
val carbon_url = uri("hg:https://bitbucket.org/viperproject/carbon#1565055c99f3b07d71f02f99de092d3077491d66")
val silicon_url = uri("hg:https://bitbucket.org/viperproject/silicon#ea1e7c4eaded6ef66b106f8f65d85d25e944a624")

scalaVersion := "2.12.7"

/*
buildDepdendencies.classpath contains the mapping from project to a list of its dependencies. The viper projects silver,
silicon and carbon specify their dependencies as a regular sbt subproject: they expect a symlink in the project root to
the relevant project. Instead, we replace those dependencies by a reference to the repository as above. So e.g.
"the silver project at hg:carbon" becomes "the silver project at hg:silver". All other dependencies are left alone.
 */
buildDependencies in Global := {
  val log = sLog.value
  val oldDeps = (buildDependencies in Global).value
  def fixDep(dep: ClasspathDep[ProjectRef]): ClasspathDep[ProjectRef] = dep.project.project match {
    case "silver" =>
      ResolvedClasspathDependency(ProjectRef(silver_url, "silver"), dep.configuration)
    case "silicon" =>
      ResolvedClasspathDependency(ProjectRef(silicon_url, "silicon"), dep.configuration)
    case "carbon" =>
      ResolvedClasspathDependency(ProjectRef(carbon_url, "carbon"), dep.configuration)
    case _ =>
      dep
  }
  val newDeps = for((proj, deps) <- oldDeps.classpath) yield (proj, deps map fixDep)
  BuildDependencies(newDeps, oldDeps.aggregate)
}

lazy val hre = project in file("hre")

lazy val silver_ref = ProjectRef(silver_url, "silver")
lazy val carbon_ref = ProjectRef(carbon_url, "carbon")
lazy val silicon_ref = ProjectRef(silicon_url, "silicon")

lazy val viper_api = (project in file("."))
  .dependsOn(silver_ref, silicon_ref, carbon_ref)
  .dependsOn(hre)
  .settings(
    name := "viper-api",
    organization := "vercors",
    version := "1.0-SNAPSHOT",

    assemblyMergeStrategy in assembly := {
      case "logback.xml" => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    }
  )
