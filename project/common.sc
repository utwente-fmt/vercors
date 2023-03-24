import $ivy.`com.lihaoyi::mill-contrib-scalapblib:`
import mill._
import define.Sources
import contrib.scalapblib.{ScalaPBModule => BaseScalaPBModule, _}
import scalalib.{ScalaModule => BaseScalaModule, JavaModule => BaseJavaModule, _}
import modules.Jvm

import os._

object Dir {
  val root = implicitly[define.Ctx].millSourcePath / os.up
  val src = root / "src"
  val test = root / "test"
  val res = root / "res"
  val lib = root / "lib"
  val docs = root / "docs"
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

trait JavaModule extends BaseJavaModule {
  def forkArgs = Seq("-Xmx2G", "-Xss20m")
}

trait ScalaModule extends BaseScalaModule with JavaModule {
  def scalaVersion = "2.13.5"
}

trait ScalaPBModule extends BaseScalaPBModule with ScalaModule {
  def scalaPBVersion = "0.11.11"
}

trait VercorsJavaModule extends JavaModule { outer =>
  def key: String
	def deps: T[Agg[Dep]]
	def sourcesDir = T { Dir.src / key }
	def sources = T.sources { sourcesDir() }
	def docResources = T.sources { Dir.docs / key }
	def unmanagedClasspath = T {
		if(os.exists(Dir.lib / key))
			Agg.from(os.list(Dir.lib / key).filter(_.ext == "jar").map(PathRef(_)))
		else Agg.empty
	}
	def ivyDeps = Deps.common ++ deps()

  def bareResources: Sources = T.sources()
  def packedResources: Sources = T.sources { Dir.res / key }
  final def resources = T.sources { bareResources() ++ packedResources() }

  private def nilSources = T.sources()

  def transitiveBareResources = T {
    T.traverse(
      (moduleDeps ++ compileModuleDeps).flatMap(_.transitiveModuleDeps).distinct
    ) {
      case module: VercorsJavaModule => module.bareResources
      case other => nilSources
    }().flatten
  }

  def localPackedClasspath = T { packedResources() ++ Agg(compile().classes) }
  def transitiveLocalPackedClasspath = T {
    T.traverse(
      (moduleDeps ++ compileModuleDeps).flatMap(_.transitiveModuleDeps).distinct
    ) {
      case module: VercorsJavaModule => module.localPackedClasspath
      case other => other.localClasspath
    }().flatten
  }

  def upstreamAssemblyClasspath = T {
    transitiveLocalPackedClasspath() ++
      unmanagedClasspath() ++
      resolvedRunIvyDeps()
  }

  def upstreamAssembly = T {
    Jvm.createAssembly(
      upstreamAssemblyClasspath().map(_.path),
      manifest(),
      assemblyRules = assemblyRules
    )
  }

  def assembly = T {
    Jvm.createAssembly(
      Agg.from(localPackedClasspath().map(_.path)),
      manifest(),
      prependShellScript(),
      Some(upstreamAssembly().path),
      assemblyRules
    )
  }
}

trait VercorsModule extends ScalaModule with VercorsJavaModule { outer =>
  trait Tests extends ScalaModuleTests with TestModule.ScalaTest with VercorsJavaModule {
    def key = outer.key
    def sourcesDir = T { Dir.test / key }
    def sources = T.sources { sourcesDir() }
    def deps = T { Agg.empty }
    def ivyDeps = Deps.common ++ Agg(ivy"org.scalatest::scalatest:3.2.7") ++ outer.deps() ++ deps()
  }
}