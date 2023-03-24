import $ivy.`com.lihaoyi::mill-contrib-buildinfo:`
import $file.project.release
import $file.project.common
import $file.project.fetchJars
import $file.project.colMeta
import $file.project.antlr
import $file.project.viper

import os._

import mill._
import scalalib._
import contrib.buildinfo.BuildInfo

import release.ReleaseModule
import common.{Dir, ScalaModule, ScalaPBModule, VercorsModule}
import viper.viper

object hre extends VercorsModule {
	def key = "hre"
	def deps = Agg(
		ivy"org.fusesource.jansi:jansi:2.4.0",
		ivy"net.harawata:appdirs:1.2.1",
		ivy"net.java.dev.jna:jna:5.13.0",
	)
	def moduleDeps = Seq(pprofProto)

	object pprofProto extends ScalaPBModule {
		def scalaPBSources = hre.sources
		def scalaPBFlatPackage = true
	}
}

object col extends VercorsModule {
	def key = "col"
	def deps = T { Agg.empty }
	def generatedSources = T { colMeta.meta.helpers() }
	def moduleDeps = Seq(hre, colMeta.proto)

	object test extends Tests
}


object parsers extends VercorsModule {
	def key = "parsers"
	def generatedSources = T.sources {
		Seq(
			antlr.c.generate(),
			antlr.java.generate(),
			antlr.pvl.generate()
		)
	}
	def deps = Agg(
		ivy"org.antlr:antlr4-runtime:4.8"
	)
	def moduleDeps = Seq(hre, col)
}

object rewrite extends VercorsModule {
	def key = "rewrite"
	def deps = Agg(
		ivy"org.sosy-lab:java-smt:3.14.3",
		ivy"com.lihaoyi::upickle:2.0.0",
	)
	def moduleDeps = Seq(hre, col)
}

object viperApi extends VercorsModule {
	def key = "viper"
	def deps = Agg(
		ivy"org.scalatest::scalatest:3.2.7"
	)
	def moduleDeps = Seq(hre, col, parsers, viper)

	object test extends Tests
}

object vercors extends VercorsModule {
	def key = "main"
	def deps = Agg(
		ivy"com.github.scopt::scopt:4.0.1",
	)
	def moduleDeps = Seq(hre, col, rewrite, parsers, viperApi, buildInfo)
	def mainClass = Some("vct.main.Main")
	def packedResources = T.sources()
	def bareResources = T.sources {
		Seq(
			PathRef(Dir.res / "universal" / "res"),
			PathRef(Dir.res / "universal" / "deps"),
		)
	}

	def classPathArgumentFile = T {
		val cpString = runClasspath().map(_.path.toString).mkString(java.io.File.pathSeparator)
		val cpArg = "-cp " + cpString
		os.write(T.dest / "classpath", cpArg)
		T.dest / "classpath"
	}

	object test extends Tests

	object buildInfo extends BuildInfo with ScalaModule {
		def buildInfoPackageName = Some("vct.main")
		def buildInfoMembers = T {
			Map(
				"name" -> "VerCors",
				"version" -> "2.0.0",
				"scalaVersion" -> scalaVersion(),
				"sbtVersion" -> "-",
				"currentBranch" -> "unknown branch",
				"currentCommit" -> "unknown commit",
				"currentShortCommit" -> "unknown commit",
				"gitHasChanges" -> "",
				"silverCommit" -> viper.silver.repo.commitish(),
				"siliconCommit" -> viper.silicon.repo.commitish(),
				"carbonCommit" -> viper.carbon.repo.commitish(),
			)
		}
	}
}

object allTests extends ScalaModule with ReleaseModule {
	def packedResources = T.sources()
	override def moduleDeps: Seq[JavaModule] = Seq(col.test, viperApi.test, vercors.test)

	def mainClass = T { Some("org.scalatest.tools.Runner") }

	def test(args: String*) = T.command {
		col.test.test(args: _*)
		viperApi.test.test(args: _*)
		vercors.test.test(args: _*)
	}
}