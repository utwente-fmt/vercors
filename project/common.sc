import $ivy.`com.lihaoyi::mill-contrib-scalapblib:`
import mill._
import contrib.scalapblib.{ScalaPBModule => BaseScalaPBModule, _}
import scalalib.{ScalaModule => BaseScalaModule, _}

import os._

object Dir {
  val root = implicitly[define.Ctx].millSourcePath / os.up
  val src = root / "src"
  val res = root / "res"
  val lib = root / "lib"
  val docs = root / "docs"
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
	def sourcesDir = T { Dir.src / key }
	def sources = T.sources { sourcesDir() }
	def resources = T.sources { Dir.res / key }
	def docResources = T.sources { Dir.docs / key }
	def unmanagedClasspath = T {
		if(os.exists(Dir.lib / key))
			Agg.from(os.list(Dir.lib / key).filter(_.ext == "jar").map(PathRef(_)))
		else Agg.empty
	}
	def ivyDeps = Deps.common ++ deps()
}