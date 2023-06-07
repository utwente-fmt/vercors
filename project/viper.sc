import $ivy.`com.lihaoyi::mill-contrib-buildinfo:`
import $file.common
import $file.fetchViper
import $file.fetchJars

import mill._
import scalalib._
import contrib.buildinfo.BuildInfo

import common.{Deps, ScalaModule}
import fetchViper.{silverGit, siliconGit, carbonGit}
import fetchJars.{z3 => z3Jar}

object viper extends ScalaModule {
  object silver extends ScalaModule {
    def scalaVersion = "2.13.10"
    def repo = silverGit
    def sources = T.sources { repo.repo() / "src" / "main" / "scala" }
    def ivyDeps = Deps.log ++ Agg(
      ivy"org.scala-lang:scala-reflect:2.13.10",
      ivy"org.scalatest::scalatest:3.1.2",
      ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2",
      ivy"com.lihaoyi::fastparse:2.2.2",
      ivy"org.rogach::scallop:4.0.4",
      ivy"commons-io:commons-io:2.8.0",
      ivy"com.google.guava:guava:29.0-jre",
      ivy"org.jgrapht:jgrapht-core:1.5.0",
    )
  }

  object silicon extends ScalaModule {
    object buildInfo extends BuildInfo with ScalaModule {
      def buildInfoPackageName = "viper.silicon"
      def buildInfoMembers = T {
        Seq(
          BuildInfo.Value("projectName", "silicon"),
          BuildInfo.Value("projectVersion", "1.1-SNAPSHOT"),
          BuildInfo.Value("scalaVersion", scalaVersion()),
          BuildInfo.Value("sbtVersion", "-"),
          BuildInfo.Value("gitRevision", silicon.repo.commitish()),
          BuildInfo.Value("gitBranch", "(detached)"),
        )
      }
    }

    object common extends ScalaModule {
      def scalaVersion = "2.13.10"
      def sources = T.sources { silicon.repo.repo() / "common" / "src" / "main" / "scala" }
      def moduleDeps = Seq(silver)
    }

    def scalaVersion = "2.13.10"
    def repo = siliconGit
    def sources = T.sources { repo.repo() / "src" / "main" / "scala" }
    def ivyDeps = Deps.log ++ Agg(
      ivy"org.apache.commons:commons-pool2:2.9.0",
      ivy"io.spray::spray-json:1.3.6",
    )
    override def resources = T.sources {
      repo.repo() / "src" / "main" / "resources"
    }
    override def unmanagedClasspath = Agg(z3Jar.classPath())
    def moduleDeps = Seq(silver, common, buildInfo)
  }

  object carbon extends ScalaModule {
    def scalaVersion = "2.13.10"
    def repo = carbonGit
    def sources = T.sources { repo.repo() / "src" / "main" / "scala" }
    def ivyDeps = Deps.log
    def moduleDeps = Seq(silver)
    def resources = T.sources { repo.repo() / "src" / "main" / "resources" }
  }

  def moduleDeps = Seq(silver, silicon, carbon)
}