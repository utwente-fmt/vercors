import $ivy.`com.lihaoyi::mill-contrib-scalapblib:`
import $file.release

import mill._
import define.Sources
import contrib.scalapblib.{ScalaPBModule => BaseScalaPBModule, _}
import scalalib.{ScalaModule => BaseScalaModule, JavaModule => BaseJavaModule, _}
import modules.Jvm
import os._

import release.ReleaseModule

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

trait VercorsJavaModule extends JavaModule with ReleaseModule { outer =>
  def key: String
	def deps: T[Agg[Dep]]
	def sourcesDir = T { Dir.src / key }
	def sources = T.sources { sourcesDir() }
  def packedResources = T.sources { Dir.res / key }
	def docResources = T.sources { Dir.docs / key }
	def unmanagedClasspath = T {
		if(os.exists(Dir.lib / key))
			Agg.from(os.list(Dir.lib / key).filter(_.ext == "jar").map(PathRef(_)))
		else Agg.empty
	}
	def ivyDeps = Deps.common ++ deps()
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