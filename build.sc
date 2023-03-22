import $ivy.`com.lihaoyi::mill-contrib-scalapblib:`
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:`

import os._
import requests._

import mill._
import scalalib.{ScalaModule => BaseScalaModule, _}
import contrib.scalapblib.{ScalaPBModule => BaseScalaPBModule, _}
import contrib.buildinfo.BuildInfo
import modules.Jvm

object Dir {
	val root = Path("/home/pieter/vercors")
	val src = root / "src"
	val res = root / "res"
	val lib = root / "lib"
	val project = root / "project"
}

object Deps {
	val log = Agg(
		ivy"com.typesafe.scala-logging::scala-logging:3.9.4",
		ivy"ch.qos.logback:logback-classic:1.2.3",
	)

	val common = log ++ Agg(
		ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
	)
}

trait ScalaModule extends BaseScalaModule {
 	def scalaVersion = "2.13.5" 
 	def forkArgs = Seq("-Xmx2G", "-Xss20m")
}

trait ScalaPBModule extends BaseScalaPBModule with ScalaModule { 
	def scalaPBVersion = "0.11.11" 
}

trait VercorsModule extends ScalaModule {
	def key: String
	def deps: T[Agg[Dep]]
	def sources = T.sources { Dir.src / key }
	def resources = T.sources { Dir.res / key }
	def unmanagedClasspath = Agg(PathRef(Dir.lib / key))
	def lib = T { Dir.lib / key }
	def ivyDeps = Deps.common ++ deps()
}

trait GitModule extends Module {
	def url: T[String]
	def commitish: T[String]

	def repo = T {
		os.proc("git", "init").call(cwd=T.dest)
		os.proc("git", "remote", "add", "origin", url()).call(cwd=T.dest)
		os.proc("git", "fetch", "--depth", "1", "origin", commitish()).call(cwd=T.dest)
		os.proc("git", "checkout", "FETCH_HEAD").call(cwd=T.dest)
		PathRef(T.dest)
	}
}

object hre extends VercorsModule {
	def key = "hre"
	def deps = Agg(
		ivy"org.fusesource.jansi:jansi:2.4.0",
		ivy"net.harawata:appdirs:1.2.1",
		ivy"net.java.dev.jna:jna:5.13.0",
	)
	def moduleDeps = Seq(pprofProto)

	object pprofProto extends ScalaPBModule {
		def scalaPBSources = T.sources { Dir.lib / key / "protobuf" }
		def scalaPBFlatPackage = true
	}
}

object col extends VercorsModule {
	def key = "col"
	def deps = T { Agg.empty }
	def sources = T.sources { super.sources() ++ meta.helpers() }
	def moduleDeps = Seq(hre, proto)

	object meta extends BaseScalaModule {
		def scalaVersion = "2.12.12"
		def sources = T.sources { Dir.project / "metabuild" }
		def ivyDeps = Agg(
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
}


object parsers extends VercorsModule {
	def key = "parsers"
	def sources = T.sources { super.sources() ++ Seq(c.generate(), java.generate(), pvl.generate()) }
	def deps = Agg(
		ivy"org.antlr:antlr4-runtime:4.8"
	)
	def unmanagedClasspath = Agg(PathRef(lib() / "SysCIR.jar"))
	def moduleDeps = Seq(hre, col)


	object antlr extends Module {
		def url = T { "https://github.com/niomaster/antlr4/releases/download/4.8-extractors-2/antlr4.jar" }

		def classPath = T {
			os.write(T.dest / "antlr.jar", requests.get.stream(url()))
			PathRef(T.dest / "antlr.jar")
		}

		def generate(target: T[PathRef], isParser: Boolean, deps: T[Seq[PathRef]]) = T.task {
			deps()
			Jvm.runSubprocess(
				mainClass = "org.antlr.v4.Tool",
				classPath = Agg(classPath().path),
				mainArgs = Nil
			)
		}
	}

	trait GenModule extends Module {
		def base = T { parsers.lib() / "antlr4" }

		def lexer: String
		def lexerRef: T[PathRef] = T { PathRef(base() / lexer) }

		def parser: String
		def parserRef: T[PathRef] = T { PathRef(base() / parser) }

		def deps: Seq[String]
		def depsRef: T[Seq[PathRef]] = T { deps.map(dep => base() / dep).map(PathRef(_)) }

		def generate = T {
			def runAntlr(target: os.Path, args: Seq[String] = Nil): Unit = {
				val mainArgs = Seq(
					"-encoding", "utf-8",
					"-package", "vct.antlr4.generated",
					"-lib", base().toString,
					"-o", T.dest.toString,
					target.toString
				) ++ args

				Jvm.runSubprocess(
					mainClass = "org.antlr.v4.Tool",
					classPath = Agg(antlr.classPath().path),
					mainArgs = mainArgs
				)
			}

			depsRef()
			runAntlr(lexerRef().path)
			runAntlr(parserRef().path, args = Seq("-listener", "-visitor", "-scala-extractor-objects"))
			PathRef(T.dest)
		}
	}

	object c extends GenModule {
		def lexer = "LangCLexer.g4"
		def parser = "CParser.g4"
		def deps = Seq(
			"SpecParser.g4", "SpecLexer.g4",
			"LangCParser.g4", "LangCLexer.g4",
			"LangOMPParser.g4", "LangOMPLexer.g4",
			"LangGPGPUParser.g4", "LangGPGPULexer.g4",
		)
	}

	object java extends GenModule {
		def lexer = "LangJavaLexer.g4"
		def parser = "JavaParser.g4"
		def deps = Seq(
			"SpecParser.g4", "SpecLexer.g4",
			"LangJavaParser.g4", "LangJavaLexer.g4",
		)
	}

	object pvl extends GenModule {
		def lexer = "LangPVLLexer.g4"
		def parser = "PVLParser.g4"
		def deps = Seq(
			"SpecParser.g4", "SpecLexer.g4",
			"LangPVLParser.g4", "LangPVLLexer.g4",
		)
	}	
}

object rewrite extends VercorsModule {
	def key = "rewrite"
	def deps = Agg(
		ivy"org.sosy-lab:java-smt:3.14.3",
		ivy"com.lihaoyi::upickle:2.0.0",
	)
	def moduleDeps = Seq(hre, col)
}

object viper extends ScalaModule {
	object silver extends ScalaModule {
		object gitSource extends GitModule {
			def url = T { "https://github.com/viperproject/silver.git" }
			def commitish = T { "11bde93e486e983141c01ac7df270e9f06e8ab06" }
		}

		def scalaVersion = "2.13.10"
		def sources = T.sources { gitSource.repo().path / "src" / "main" / "scala" }
		def ivyDeps = Deps.log ++ Agg(
			ivy"org.scala-lang:scala-reflect:2.13.10",
			ivy"org.scalatest::scalatest:3.1.2",
			ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2",
			ivy"com.lihaoyi::fastparse:2.2.2",
			ivy"org.rogach::scallop:4.0.4",
			ivy"commons-io:commons-io:2.8.0",
			ivy"com.google.guava:guava:29.0-jre",
			ivy"org.jgrapht:jgrapht-core:1.5.0",
			ivy"org.slf4j:slf4j-api:1.7.30",
		)
	}

	object silicon extends ScalaModule {
		object gitSource extends GitModule {
			def url = T { "https://github.com/viperproject/silicon.git" }
			def commitish = T { "f844927fe6f54c3dbc5adbccfa011034c8036640" }
		}

		object buildInfo extends BuildInfo with ScalaModule {
			def buildInfoPackageName = Some("viper.silicon")
			def buildInfoMembers = T {
				Map(
					"projectName" -> "silicon",
					"projectVersion" -> "1.1-SNAPSHOT",
					"scalaVersion" -> scalaVersion(),
					"sbtVersion" -> "-",
					"gitRevision" -> gitSource.commitish(),
					"gitBranch" -> "(detached)",
				)
			}
		}

		object common extends ScalaModule {
			def scalaVersion = "2.13.10"
			def sources = T.sources { gitSource.repo().path / "common" / "src" / "main" / "scala" }
			def moduleDeps = Seq(silver)
		}

		object z3Jar extends Module {
			def url = T { "https://www.sosy-lab.org/ivy/org.sosy_lab/javasmt-solver-z3/com.microsoft.z3-4.8.7.jar" }
			def classPath = T {
				os.write(T.dest / "z3.jar", requests.get.stream(url()))
				PathRef(T.dest / "z3.jar")
			}
		}

		def scalaVersion = "2.13.10"
		def sources = T.sources { gitSource.repo().path / "src" / "main" / "scala" }
		def ivyDeps = Deps.log ++ Agg(
		    ivy"org.apache.commons:commons-pool2:2.9.0",
		    ivy"io.spray::spray-json:1.3.6",
		)
		override def resources = T.sources {
			gitSource.repo().path / "src" / "main" / "resources"
		}
		override def unmanagedClasspath = Agg(z3Jar.classPath())
		def moduleDeps = Seq(silver, common, buildInfo)
	}

	object carbon extends ScalaModule {
		object gitSource extends GitModule {
			def url = T { "https://github.com/viperproject/carbon.git" }
			def commitish = T { "44f9225dcde2374c3b8051b6d56ac88c7c4ffdd5" }
		}

		def scalaVersion = "2.13.10"
		def sources = T.sources { gitSource.repo().path / "src" / "main" / "scala" }
		def ivyDeps = Deps.log
		def moduleDeps = Seq(silver)
		def resources = T.sources { gitSource.repo().path / "src" / "main" / "resources" }
	}

	def moduleDeps = Seq(silver, silicon, carbon)
}

object viperApi extends VercorsModule {
	def key = "viper"
	def deps = Agg(
		ivy"org.scalatest::scalatest:3.2.7"
	)
	def moduleDeps = Seq(hre, col, parsers, viper)
}

object vercors extends VercorsModule {
	def key = "main"
	def deps = Agg(
		ivy"com.github.scopt::scopt:4.0.1",
	)
	def moduleDeps = Seq(hre, col, rewrite, parsers, viperApi, buildInfo)
	def mainClass = Some("vct.main.Main")
	def resources = T.sources {
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
				"silverCommit" -> viper.silver.gitSource.commitish(),
				"siliconCommit" -> viper.silicon.gitSource.commitish(),
				"carbonCommit" -> viper.carbon.gitSource.commitish(),
			)
		}
	}
}