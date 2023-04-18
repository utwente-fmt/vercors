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
    ivy"com.typesafe.scala-logging::scala-logging:3.9.5",
    ivy"ch.qos.logback:logback-classic:1.4.5",
  )

  val common = log ++ Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
    ivy"io.spray::spray-json:1.3.6"
  )
}

trait JavaModule extends BaseJavaModule {
  def forkArgs = Seq("-Xmx2G", "-Xss20m")

  def classPathFileElements = T { runClasspath().map(_.path.toString) }

  def unixClassPathArgumentFile = T {
    val cpString = classPathFileElements().mkString(":")
    val cpArg = "-cp " + cpString
    os.write(T.dest / "classpath", cpArg)
    T.dest / "classpath"
  }

  def windowsClassPathArgumentFile = T {
    val cpString = classPathFileElements().mkString(";")
    val cpArg = "-cp " + cpString
    os.write(T.dest / "classpath", cpArg)
    T.dest / "classpath"
  }

  def runScriptClasses = T {
    Map(
      "run" -> finalMainClass(),
    )
  }

  def runScript = T {
    // PB: this is nearly just Jvm.createLauncher, but you cannot set the filename, and uses a literal classpath instead of a file.
    for((name, mainClass) <- runScriptClasses()) {
      // thanks https://gist.github.com/lhns/ee821a5cd1b2031856b21a0e78e1ecc9
      val header = "@ 2>/dev/null # 2>nul & echo off & goto BOF"
      val unix = Seq(
        ":",
        s"java ${forkArgs().mkString(" ")} @${unixClassPathArgumentFile()} $mainClass \"$$@\"",
        "exit",
      )
      val batch = Seq(
        ":BOF",
        s"java ${forkArgs().mkString(" ")} @${windowsClassPathArgumentFile()} $mainClass %*",
        "exit /B %errorlevel%",
      )
      val script = header + "\r\n" + unix.mkString("\n") + "\n\r\n" + batch.mkString("\r\n") + "\r\n"
      val isWin = scala.util.Properties.isWin
      val wantBatch = isWin && !org.jline.utils.OSUtils.IS_CYGWIN && !org.jline.utils.OSUtils.IS_MSYSTEM
      val fileName = if(wantBatch) name + ".bat" else name
      os.write(T.dest / fileName, script)
      if(!isWin) os.perms.set(T.dest / name, os.PermSet.fromString("rwxrwxr-x"))
    }
    T.dest
  }
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

  def classPathFileElements = T { runClasspathElements() }
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