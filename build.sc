import $meta._
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:`
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:`

import os._
import mill._
import scalalib.{JavaModule => BaseJavaModule, ScalaModule => BaseScalaModule, _}
import contrib.scalapblib.{ScalaPBModule => BaseScalaPBModule, _}
import contrib.buildinfo.BuildInfo
import mill.util.Jvm
import vct.col.ast.structure
import vct.col.ast.structure.{AllFamilies, FamilyDefinition, Name, NodeDefinition}

import scala.util.control.NonFatal

object settings {
  val root = implicitly[define.Ctx].millSourcePath
  val src = root / "src"
  val test = root / "test"
  val res = root / "res"
  val lib = root / "lib"
  val docs = root / "docs"
  val meta = root / "out" / "mill-build"

  object deps {
    val log = Agg(
      ivy"com.typesafe.scala-logging::scala-logging:3.9.5",
      ivy"ch.qos.logback:logback-classic:1.4.5",
    )

    val common = log ++ Agg(
      ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
      ivy"io.spray::spray-json:1.3.6"
    )
  }
}

object util {
  case class DataPoint(base: Path, coordinate: os.SubPath) {
    lazy val mtime = os.mtime(base / coordinate)

    override def equals(obj: Any): Boolean = obj match {
      case other: DataPoint => coordinate == other.coordinate
    }

    private lazy val _hashCode = coordinate.hashCode()
    override def hashCode(): Int = _hashCode

    def copyTo(dest: Path): Unit = {
      os.copy(base / coordinate, dest / coordinate, createFolders = true)
    }

    def copyOver(dest: Path): Unit =
      if(DataPoint(dest, coordinate).mtime != mtime)
        os.copy.over(base / coordinate, dest / coordinate)

    def delete(): Unit =
      os.remove(base / coordinate)
  }

  def quickCopy(target: Path, sourcePaths: Seq[PathRef]): Unit = {
    val sources =
      sourcePaths
        .map(_.path)
        .flatMap { base =>
          os.walk.stream(base)
            .filter(os.isFile(_))
            .map(p => DataPoint(base, p.subRelativeTo(base)))
            .toSeq
        }
        .toSet

    val targets =
      os.walk.stream(target)
        .filter(os.isFile(_))
        .map(p => DataPoint(target, p.subRelativeTo(target)))
        .toSet

    for(toRemove <- targets if !sources.contains(toRemove)) {
      toRemove.delete()
    }

    for(toWrite <- sources if !targets.contains(toWrite)) {
      toWrite.copyTo(target)
    }

    for(toWriteOver <- sources if targets.contains(toWriteOver)) {
      toWriteOver.copyOver(target)
    }
  }

  trait JavaModule extends BaseJavaModule {
    // https://github.com/viperproject/silicon/issues/748
    // 32MB is enough stack space for silicon, a 100% marco guarantee
    override def forkArgs = Seq("-Xmx2G", "-Xss32m")

    def classPathFileElements = T { runClasspath().map(_.path.toString) }

    def unixClassPathArgumentFile = T {
      val cpString = classPathFileElements().mkString(":")
      val cpArg = "-cp " + cpString
      os.write(T.dest / "classpath", cpArg)
      T.dest / "classpath"
    }

    def strictOptionsFile = T.source {
      settings.root / ".compile-strict"
    }

    def strictOptions: T[Boolean] = T {
      os.exists(strictOptionsFile().path)
    }

    override def javacOptions = T {
      val shared = Seq(
        "--release", "17",
        "-deprecation",
      )

      if(strictOptions()) {
        Seq(
          "-Werror",
        ) ++ shared
      } else {
        Seq (
          // nothing here yet
        ) ++ shared
      }
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
        val quote = "\""
        val header = "@ 2>/dev/null # 2>nul & echo off & goto BOF"
        val unix = Seq(
          ":",
          s"java ${forkArgs().mkString(" ")} @${unixClassPathArgumentFile()} $mainClass $quote$$@$quote",
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
    def scalaVersion = "2.13.12"

    override def scalacOptions = T {
      val shared = Seq(
        "-deprecation",
        "-feature",
        "-Xno-patmat-analysis"
      )

      if (strictOptions()) {
        Seq(
          "-Ypatmat-exhaust-depth", "off",
          "-Werror",
        ) ++ shared
      } else {
        Seq(
          "-Xno-patmat-analysis",
        ) ++ shared
      }
    }
  }

  trait ScalaPBModule extends BaseScalaPBModule with ScalaModule {
    def scalaPBVersion = "0.11.11"

    def scalaPBFlatPackage = true

    override def scalaPBClasspath: T[mill.api.Loose.Agg[PathRef]] = T {
      mill.scalalib.Lib.resolveDependencies(
        Seq(
          coursier.LocalRepositories.ivy2Local,
          coursier.MavenRepository("https://repo1.maven.org/maven2")
        ),
        Seq(
          ivy"com.thesamet.scalapb::scalapbc:${scalaPBVersion()}",
        )
          .map(Lib.depToBoundDep(_, "2.13.1"))
      ).map(_.map(_.withRevalidateOnce))
    }

    def scalaPBSearchDeps = true
  }

  trait SeparatePackedResourcesModule extends JavaModule {
    def bareResourcePaths: T[Seq[Path]] = T { Seq.empty[Path] }
    def bareResources = T.sources { bareResourcePaths().map(PathRef(_, quick = true)) }

    def packedResources: T[Seq[PathRef]]

    override final def resources = T.sources {
      bareResources() ++ packedResources()
    }

    private def nilTask: T[Seq[Path]] = T { Seq.empty[Path] }

    def transitiveBareResourcePaths = T {
      T.traverse(
        (moduleDeps ++ compileModuleDeps).flatMap(_.transitiveModuleDeps).distinct
      ) {
        case module: SeparatePackedResourcesModule => module.bareResourcePaths
        case other => nilTask
      }().flatten
    }

    def bareClasspath = T {
      bareResourcePaths() ++ transitiveBareResourcePaths()
    }

    def localPackedClasspath = T {
      localCompileClasspath().toSeq ++ packedResources() ++ Agg(compile().classes)
    }

    def transitiveLocalPackedClasspath = T {
      (T.traverse(
        (moduleDeps ++ compileModuleDeps).flatMap(_.transitiveModuleDeps).distinct
      ) {
        case module: SeparatePackedResourcesModule => module.localPackedClasspath
        case m => m.localClasspath
      })().flatten
    }

    override def upstreamAssembly = T {
      Assembly.createAssembly(
        (transitiveLocalPackedClasspath() ++
          unmanagedClasspath() ++
          resolvedRunIvyDeps()
        ).map(_.path),
        manifest(),
        assemblyRules = assemblyRules
      )
    }

    override def assembly = T {
      Assembly.createAssembly(
        Agg.from(localPackedClasspath().map(_.path)),
        manifest(),
        prependShellScript(),
        Some(upstreamAssembly().path),
        assemblyRules
      )
    }
  }

  trait ReleaseModule extends JavaModule with SeparatePackedResourcesModule {
    def name: T[String] = T { this.getClass.getSimpleName.replace("$", "").capitalize }
    def executableName: T[String] = T { name().toLowerCase }
    def version: T[String] = T { "0.1-SNAPSHOT" }
    def maintainer: T[String] = T { "Pieter Bos <p.h.bos@utwente.nl>" }
    def summary: T[String] = T { s"${name()} test build" }
    def description: T[String] = T { s"${name()} test build" }
    def homepage: T[Option[String]] = T { None }

    def debianPackageName: T[String] = T { name().toLowerCase }
    def debianSection: T[String] = T { "java" }
    def debianArchitecture: T[String] = T { "all" }

    def winExecutableName: T[String] = T { executableName() + ".bat" }

    private def copy(from: Path, to: Path): Path = {
      os.copy(from, to, followLinks = true, replaceExisting = false, createFolders = true, mergeFolders = true)
      to
    }

    def release() = T.command {
      Map(
        "unix" -> unixTar().path,
        "macos" -> macosTar().path,
        "win" -> winZip().path,
        "deb" -> deb().path,
      )
    }

    def unixTar = T {
      val dest = T.dest / "dest"

      val jar = copy(assembly().path, dest / s"${executableName()}.jar")
      val res = bareClasspath().map(res => copy(res, dest / res.last))

      os.walk(dest / "deps" / "win", preOrder = false).foreach(os.remove)
      os.walk(dest / "deps" / "darwin", preOrder = false).foreach(os.remove)

      os.write(dest / ".classpath", "-cp " + (jar +: res).map(_.relativeTo(dest)).map(_.toString).mkString(":"))

      os.write(dest / executableName(),
        s"""#!/bin/sh
           |HERE=$$(dirname $$(readlink -f $$0))
           |(cd $$HERE; java ${forkArgs().mkString(" ")} @${os.rel / ".classpath"} ${finalMainClass()} "$$@")
           |""".stripMargin)
      os.perms.set(dest / executableName(), os.PermSet.fromString("rwxrwxr-x"))

      val out = T.dest / s"${executableName()}-${version()}-unix.tar.xz"
      os.proc("tar", "-cJf", out, os.list(dest).map(_.relativeTo(dest))).call(cwd=dest)
      PathRef(out)
    }

    def macosTar = T {
      val dest = T.dest / "dest"

      val jar = copy(assembly().path, dest / s"${executableName()}.jar")
      val res = bareClasspath().map(res => copy(res, dest / res.last))

      os.walk(dest / "deps" / "unix", preOrder = false).foreach(os.remove)
      os.walk(dest / "deps" / "win", preOrder = false).foreach(os.remove)

      os.write(dest / ".classpath", "-cp " + (jar +: res).map(_.relativeTo(dest)).map(_.toString).mkString(":"))

      os.write(dest / executableName(),
        s"""#!/bin/sh
           |HERE=$$(dirname $$(readlink -f $$0))
           |(cd $$HERE; java ${forkArgs().mkString(" ")} @${os.rel / ".classpath"} ${finalMainClass()} "$$@")
           |""".stripMargin)
      os.perms.set(dest / executableName(), os.PermSet.fromString("rwxrwxr-x"))

      val out = T.dest / s"${executableName()}-${version()}-macos.tar.xz"
      os.proc("tar", "-cJf", out, os.list(dest).map(_.relativeTo(dest))).call(cwd = dest)
      PathRef(out)
    }

    def winZip = T {
      val dest = T.dest / "dest"

      val jar = copy(assembly().path, dest / s"${executableName()}.jar")
      val res = bareClasspath().map(res => copy(res, dest / res.last))

      os.walk(dest / "deps" / "unix", preOrder = false).foreach(os.remove)
      os.walk(dest / "deps" / "darwin", preOrder = false).foreach(os.remove)

      os.write(dest / ".classpath", "-cp " + (jar +: res).map(_.relativeTo(dest)).map(_.toString).mkString(":"))

      os.write(dest / winExecutableName(),
        s"""@echo off
           |cd /D "%~dp0"
           |java ${forkArgs().mkString(" ")} @${(os.rel / ".classpath").toString} ${finalMainClass()} %*
           |""".stripMargin)

      val out = T.dest / s"${executableName()}-${version()}-win.zip"
      os.proc("zip", out, "-r", os.list(dest).map(_.relativeTo(dest))).call(cwd = dest)
      PathRef(out)
    }

    def deb = T {
      val outName = s"${debianPackageName()}-debian-${version()}"
      val root = T.dest / outName
      os.makeDir(root)
      val dest = root / "usr" / "share" / debianPackageName()
      val fromDebRoot = (p: Path) => os.root / p.relativeTo(root)

      val jar = copy(assembly().path, dest / s"${executableName()}.jar")
      val res = bareClasspath().map(res => copy(res, dest / res.last))

      os.walk(dest / "deps" / "win", preOrder = false).foreach(os.remove)
      os.walk(dest / "deps" / "darwin", preOrder = false).foreach(os.remove)

      os.write(dest / ".classpath", "-cp " + (jar +: res).map(fromDebRoot).map(_.toString).mkString(":"))

      os.write(dest / executableName(),
        s"""#!/bin/sh
           |java ${forkArgs().mkString(" ")} @${fromDebRoot(dest / ".classpath")} ${finalMainClass()} "$$@"
           |""".stripMargin)
      os.perms.set(dest / executableName(), os.PermSet.fromString("rwxrwxr-x"))

      os.makeDir(root / "usr" / "bin")
      os.symlink(root / "usr" / "bin" / executableName(), fromDebRoot(dest / executableName()))

      os.makeDir(root / "DEBIAN")
      os.write(root / "DEBIAN" / "control",
        s"""Package: ${debianPackageName()}
           |Version: ${version()}
           |Section: ${debianSection()}
           |Architecture: ${debianArchitecture()}
           |Maintainer: ${maintainer()}${homepage().map(homepage => s"\nHomepage: $homepage").getOrElse("")}
           |Description: ${summary()}
           |${description().split('\n').mkString(" ", "\n ", "")}
           |""".stripMargin)

      os.proc("dpkg-deb", "--root-owner-group", "-Zxz", "--build", root.relativeTo(T.dest)).call(cwd = T.dest)
      PathRef(T.dest / s"$outName.deb")
    }
  }

  trait VercorsJavaModule extends JavaModule with ReleaseModule { outer =>
    def key: String
    def deps: T[Agg[Dep]]
    def sourcesDir = T { settings.src / key }
    override def sources = T.sources { sourcesDir() }
    def packedResources = T.sources { settings.res / key }
    override def docResources = T.sources { settings.docs / key }
    override def unmanagedClasspath = T {
      if(os.exists(settings.lib / key))
        Agg.from(os.list(settings.lib / key).filter(_.ext == "jar").map(PathRef(_)))
      else Agg.empty
    }
    override def ivyDeps = settings.deps.common ++ deps()

    override def classPathFileElements = T { runClasspath().map(_.path.toString) }
  }

  trait VercorsModule extends ScalaModule with VercorsJavaModule { outer =>
    trait Tests extends ScalaTests with TestModule.ScalaTest with VercorsJavaModule {
      def key = outer.key
      override def sourcesDir = T { settings.test / key }
      override def sources = T.sources { sourcesDir() }
      def deps = T { Agg.empty }
      override def ivyDeps = settings.deps.common ++ Agg(ivy"org.scalatest::scalatest:3.2.7") ++ outer.deps() ++ deps()
    }
  }

  trait GitModule extends Module {
    def url: T[String]

    def commitish: T[String]

    def repo = T {
      os.proc("git", "init", "-q").call(cwd = T.dest)
      os.proc("git", "remote", "add", "origin", url()).call(cwd = T.dest)
      os.proc("git", "fetch", "--depth", "1", "origin", commitish()).call(cwd = T.dest)
      os.proc("git", "config", "advice.detachedHead", "false").call(cwd = T.dest)
      os.proc("git", "checkout", "FETCH_HEAD").call(cwd = T.dest)
      os.walk(T.dest).foreach(_.toIO.setWritable(true))
      os.remove.all(T.dest / ".git")
      T.dest
    }
  }
}

import util._

object external extends Module {
  object z3 extends Module {
    def url = T { "https://www.sosy-lab.org/ivy/org.sosy_lab/javasmt-solver-z3/com.microsoft.z3-4.8.7.jar" }

    def classPath = T {
      os.write(T.dest / "z3.jar", requests.get.stream(url()))
      PathRef(T.dest / "z3.jar")
    }
  }

  object antlr extends Module {
    def url = T {
      "https://github.com/pieter-bos/antlr4/releases/download/4.8-extractors-2/antlr4.jar"
    }

    def classPath = T {
      os.write(T.dest / "antlr.jar", requests.get.stream(url()))
      PathRef(T.dest / "antlr.jar")
    }
  }
}

object viper extends ScalaModule {
  object silverGit extends GitModule {
    def url = T { "https://github.com/viperproject/silver.git" }
    def commitish = T { "31c94df4f9792046618d9b4db52444ffe9c7c988" }
    def filteredRepo = T {
      val workspace = repo()
      os.remove.all(workspace / "src" / "test")
      workspace
    }
  }

  object siliconGit extends GitModule {
    def url = T { "https://github.com/viperproject/silicon.git" }
    def commitish = T { "529d2a49108b954d2b0749356faf985d622f54f0" }
    def filteredRepo = T {
      val workspace = repo()
      os.remove.all(workspace / "src" / "test")
      workspace
    }
  }

  object carbonGit extends GitModule {
    def url = T { "https://github.com/viperproject/carbon.git" }
    def commitish = T { "d7ac8b000e1123a72cbdda0c7679ab88ca8a52d4" }
  }

  object silver extends ScalaModule {
    override def scalaVersion = "2.13.10"
    override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
    def repo = silverGit
    override def sources = T.sources { repo.filteredRepo() / "src" / "main" / "scala" }
    override def resources = T.sources { repo.filteredRepo() / "src" / "main" / "resources" }
    override def ivyDeps = settings.deps.log ++ Agg(
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
      override def buildInfoMembers = T {
        Seq(
          BuildInfo.Value("projectName", "silicon"),
          BuildInfo.Value("projectVersion", "1.1-SNAPSHOT"),
          BuildInfo.Value("scalaVersion", silicon.scalaVersion()),
          BuildInfo.Value("sbtVersion", "-"),
          BuildInfo.Value("gitRevision", silicon.repo.commitish()),
          BuildInfo.Value("gitBranch", "(detached)"),
        )
      }
    }

    object common extends ScalaModule {
      override def scalaVersion = "2.13.10"
      override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
      override def sources = T.sources { silicon.repo.filteredRepo() / "common" / "src" / "main" / "scala" }
      override def moduleDeps = Seq(silver)
    }

    override def scalaVersion = "2.13.10"
    override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
    def repo = siliconGit
    override def sources = T.sources { repo.filteredRepo() / "src" / "main" / "scala" }
    override def ivyDeps = settings.deps.log ++ Agg(
      ivy"org.apache.commons:commons-pool2:2.9.0",
      ivy"io.spray::spray-json:1.3.6",
    )
    override def resources = T.sources {
      repo.filteredRepo() / "src" / "main" / "resources"
    }
    override def unmanagedClasspath = Agg(external.z3.classPath())
    override def moduleDeps = Seq(silver, common, buildInfo)
  }

  object carbon extends ScalaModule {
    override def scalaVersion = "2.13.10"
    override def scalacOptions = T { Seq("-Xno-patmat-analysis", "-nowarn") }
    def repo = carbonGit
    override def sources = T.sources { repo.repo() / "src" / "main" / "scala" }
    override def ivyDeps = settings.deps.log
    override def moduleDeps = Seq(silver)
    override def resources = T.sources { repo.repo() / "src" / "main" / "resources" }
  }

  override def moduleDeps = Seq(silver, silicon, carbon)
}

object vercors extends Module {
  def read[T: upickle.default.ReadWriter](p: PathRef): T =
    upickle.default.read[T](p.path.toNIO)

  def readAndWatch[T: upickle.default.ReadWriter](p: Path): T =
    upickle.default.read[T](interp.watch(p).toNIO)

  object hre extends VercorsModule {
    def key = "hre"
    def deps = Agg(
      ivy"org.fusesource.jansi:jansi:2.4.0",
      ivy"net.harawata:appdirs:1.2.1",
      ivy"net.java.dev.jna:jna:5.13.0",
    )
    override def moduleDeps = Seq(pprofProto)

    object pprofProto extends ScalaPBModule {
      override def scalaPBSources = hre.sources
      override def scalaPBFlatPackage = true
    }
  }

  object col extends VercorsModule {
    object helpers extends Module {
      def analysis: Path = settings.meta / "analyseNodeDeclarations.dest"
      def structureClasspathDir: Path = settings.meta / "structureClassPath.dest"

      val familyCross = readAndWatch[Seq[Name]](analysis / "cross-family.json")
      val nodeCross = readAndWatch[Seq[Name]](analysis / "cross-node.json")

      def allFamiliesSource = T.source(analysis / "all-families.json")
      def allFamilies = T { read[AllFamilies](allFamiliesSource()) }
      def allDefinitionsSource = T.source(analysis / "all-definitions.json")
      def allDefinitions = T { read[Seq[NodeDefinition]](allDefinitionsSource()) }

      object generators extends VercorsModule {
        def key = "helpers"
        def deps: T[Agg[Dep]] = T {
          Agg(
            ivy"org.scalameta::scalameta:4.4.9",
          )
        }
        def structureClasspath = T.source(structureClasspathDir / "classpath.json")
        def unmanagedClasspath = T { read[Seq[Path]](structureClasspath()).map(PathRef(_)) }
      }

      def instantiate[T](name: String): Task[T] = T.task {
        mill.api.ClassLoader
          .create(
            generators.runClasspath().map(_.path.toIO.toURI.toURL),
            parent = classOf[helpers.type].getClassLoader,
          )
          .loadClass(s"vct.col.ast.helpers.generator.$name")
          .asInstanceOf[Class[T]]
          .getDeclaredConstructor()
          .newInstance()
      }

      object implTraits extends Module {
        val root = settings.src / "col" / "vct" / "col" / "ast"

        def inputs = T.sources(os.walk(root).filter(_.last.endsWith("Impl.scala")).map(PathRef(_)))

        private def nodeName(p: PathRef): String =
          p.path.last.dropRight("Impl.scala".length)

        def generator = T.worker(instantiate[structure.ImplTraitGenerator]("ImplTrait"))

        def allDefinitionsSet = T {
          allDefinitions().map(_.name.base).toSet
        }

        def allFamiliesSet = T {
          val allFamiliesVal = allFamilies()
          (allFamiliesVal.declaredFamilies ++ allFamiliesVal.structuralFamilies)
            .map(_.base).toSet
        }

        def fix = T {
          val gen = generator()
          val defnSet = allDefinitionsSet()
          val familySet = allFamiliesSet()
          inputs().foreach { input =>
            val name = nodeName(input)
            val opsNames =
              (if(defnSet.contains(name)) Seq(name + "Ops") else Nil) ++
                (if(familySet.contains(name)) Seq(name + "FamilyOps") else Nil)

            try {
              gen.fix(input.path.toNIO, opsNames)
            } catch {
              case NonFatal(_) => // ok, it's best effort.
            }
          }
        }

        def generate: T[Unit] = T {
          val gen = generator()
          val defnSet = allDefinitionsSet()
          val familySet = allFamiliesSet()
          val ungenerated = (defnSet ++ familySet) -- inputs().map(nodeName)
          ungenerated.foreach { node =>
            os.makeDir.all(root / "unsorted")
            gen.generate(
              p = (root / "unsorted" / (node + "Impl.scala")).toNIO,
              node = node,
              concrete = defnSet.contains(node),
              family = familySet.contains(node),
            )
          }

          if(ungenerated.nonEmpty)
            T.log.error({
              val items = ungenerated.map(name => s" - ${name}Impl").mkString("\n")
              s"""
                 |New {Node}Impl stubs have been generated for these nodes:
                 |$items
                 |Please make sure that the appropriate {Node} extends its corresponding {Node}Impl - you can do that before it exists.
                 |If not, compilation will fail after this message.""".stripMargin
            })

          ()
        }

        def run = T {
          fix()
          generate()
        }
      }

      def protoAuxTypes = T.worker {
        Seq(
          instantiate[structure.AllNodesGenerator]("ProtoAuxTypes")(),
          instantiate[structure.AllNodesGenerator]("RewriteHelpers")(),
        )
      }

      def allFamiliesGenerators = T.worker {
        Seq(
          instantiate[structure.AllFamiliesGenerator]("AbstractRewriter")(),
          instantiate[structure.AllFamiliesGenerator]("AllScopes")(),
          instantiate[structure.AllFamiliesGenerator]("AllFrozenScopes")(),
          instantiate[structure.AllFamiliesGenerator]("BaseCoercingRewriter")(),
          instantiate[structure.AllFamiliesGenerator]("BaseNonLatchingRewriter")(),
          instantiate[structure.AllFamiliesGenerator]("SuccessorsProvider")(),
        )
      }

      def familyGenerators = T.worker {
        Seq(
          instantiate[structure.FamilyGenerator]("DeserializeFamily")(),
          instantiate[structure.FamilyGenerator]("DeclareFamily")(),
          instantiate[structure.FamilyGenerator]("OpsFamily")(),

          instantiate[structure.FamilyGenerator]("ProtoFamily")(),
        )
      }

      def nodeGenerators = T.worker {
        Seq(
          instantiate[structure.NodeGenerator]("Compare")(),
          instantiate[structure.NodeGenerator]("Rewrite")(),
          instantiate[structure.NodeGenerator]("Serialize")(),
          instantiate[structure.NodeGenerator]("Subnodes")(),
          instantiate[structure.NodeGenerator]("Ops")(),

          instantiate[structure.NodeGenerator]("Deserialize")(),
          instantiate[structure.NodeGenerator]("ProtoNode")(),
        )
      }

      object global extends Module {
        def generate = T {
          val allFamiliesVal = allFamilies()
          val declarationFamilies = allFamiliesVal.declaredFamilies
          val structuralFamilies = allFamiliesVal.structuralFamilies
          val definitions = allDefinitions()
          allFamiliesGenerators().foreach { gen =>
            T.log.debug(s"Generating ${gen.getClass.getSimpleName}")
            gen.generate(T.dest.toNIO, declarationFamilies, structuralFamilies)
          }
          protoAuxTypes().foreach(_.generate(T.dest.toNIO, definitions))
          Seq(PathRef(T.dest, quick = true))
        }
      }

      implicit object FamilySegments extends Cross.ToSegments[(structure.Name, structure.NodeKind, Seq[structure.Name])](v => implicitly[Cross.ToSegments[structure.Name]].convert(v._1))

      object family extends Cross[FamilyCross](familyCross)
      trait FamilyCross extends Cross.Module[Name] {
        def source = T.source(PathRef(analysis / "cross-family" / crossValue.path / "family.json", quick = true))
        def sourceDefn = T { read[FamilyDefinition](source()) }
        def generate = T {
          val defn = sourceDefn()
          familyGenerators().foreach { gen =>
            T.log.debug(s"Generating ${gen.getClass.getSimpleName} for ${defn.name.base}")
            gen.generate(T.dest.toNIO, defn.name, defn.kind, defn.nodes)
          }
          PathRef(T.dest, quick = true)
        }
      }

      object node extends Cross[NodeCross](nodeCross)
      trait NodeCross extends Cross.Module[Name] {
        def source = T.source(PathRef(analysis / "cross-node" / crossValue.path / "node.json", quick = true))
        def sourceDefn = T { read[NodeDefinition](source()) }
        def generate = T {
          val defn = sourceDefn()
          nodeGenerators().foreach { gen =>
            T.log.debug(s"Generating ${gen.getClass.getSimpleName} for ${defn.name.base}")
            gen.generate(T.dest.toNIO, defn)
          }
          PathRef(T.dest, quick = true)
        }
      }

      def generatedSources: T[Seq[PathRef]] = T {
        T.traverse(nodeCross)(node(_).generate)() ++
          T.traverse(familyCross)(family(_).generate)() ++
          global.generate()
      }

      def sources: T[PathRef] = T.persistent {
        util.quickCopy(T.dest, generatedSources())
        PathRef(T.dest, quick = true)
      }
    }

    def key = "col"
    def deps = T { Agg.empty }
    override def sources = T {
      helpers.implTraits.run()
      super.sources()
    }
    override def generatedSources = T { Seq(helpers.sources()) }
    override def moduleDeps = Seq(hre, serialize)

    object test extends Tests
  }

  object serialize extends VercorsModule with ScalaPBModule {
    override def scalaPBSources = T {
      col.helpers.sources() +: this.sources()
    }
    override def key: String = "serialize"
    override def deps: T[Agg[Dep]] = T { Agg.empty }
  }

  object parsers extends VercorsModule {
    def key = "parsers"
    override def generatedSources = T.sources {
      Seq(
        c.generate(),
        cpp.generate(),
        java.generate(),
        pvl.generate(),
        llvm.generate(),
      )
    }
    def deps = Agg(
      ivy"org.antlr:antlr4-runtime:4.8"
    )
    override def moduleDeps = Seq(hre, col, serialize)

    trait GenModule extends Module {
      def base = T { settings.src / "parsers" / "antlr4" }

      def lexer: String
      final def lexerRef = T.sources { base() / lexer }

      def parser: String
      final def parserRef = T.sources { base() / parser }

      def deps: Seq[String]
      final def depsRef = T.sources { deps.map(dep => base() / dep).map(PathRef(_)) }

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
            classPath = Agg(external.antlr.classPath().path),
            mainArgs = mainArgs
          )
        }

        depsRef()
        lexerRef()
        parserRef()

        runAntlr(base() / lexer)
        runAntlr(base() / parser, args = Seq("-listener", "-visitor", "-scala-extractor-objects"))
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

    object cpp extends GenModule {
      def lexer = "LangCPPLexer.g4"
      def parser = "CPPParser.g4"
      def deps = Seq(
        "SpecParser.g4", "SpecLexer.g4",
        "LangCPPParser.g4", "LangCPPLexer.g4"
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

    object llvm extends GenModule {
      def lexer = "LangLLVMSpecLexer.g4"
      def parser = "LLVMSpecParser.g4"
      def deps = Seq(
        "SpecParser.g4", "SpecLexer.g4",
        "LangLLVMSpecParser.g4", "LangLLVMSpecLexer.g4"
      )
    }
  }

  object rewrite extends VercorsModule {
    def key = "rewrite"
    def deps = Agg(
      ivy"org.sosy-lab:java-smt:3.14.3",
      ivy"com.lihaoyi::upickle:2.0.0",
      ivy"org.antlr:antlr4-runtime:4.8",
    )
    override def moduleDeps = Seq(hre, col)
  }

  object viperApi extends VercorsModule {
    def key = "viper"
    def deps = Agg(
      ivy"org.scalatest::scalatest:3.2.7"
    )
    override def moduleDeps = Seq(hre, col, parsers, viper)

    object test extends Tests
  }

  object main extends VercorsModule {
    def key = "main"
    def deps = Agg(
      ivy"com.github.scopt::scopt:4.0.1",
    )
    override def moduleDeps = Seq(hre, col, rewrite, parsers, viperApi, buildInfo)
    override def mainClass = Some("vct.main.Main")
    override def runScriptClasses = T { Map (
      "vercors" -> "vct.main.Main",
      "carbon" -> "viper.carbon.Carbon",
      "silicon" -> "viper.silicon.SiliconRunner",
      "bashOptions" -> "vct.options.BashCompletion",
    ) }
    override def packedResources = T.sources()
    override def bareResourcePaths = T {
      Seq(
        settings.res / "universal" / "res",
        settings.res / "universal" / "deps",
      )
    }

    object test extends Tests

    object buildInfo extends BuildInfo with ScalaModule {
      def callOrElse(command: Shellable*)(alt: => String): String =
        try {
          os.proc(command: _*).call().out.text().trim
        } catch {
          case _: SubprocessException => alt
        }

      def gitBranch = T.input { callOrElse("git", "rev-parse", "--abbrev-ref=strict", "HEAD")("unknown") }
      def gitCommit = T.input { callOrElse("git", "rev-parse", "HEAD")("unknown") }
      def gitShortCommit = T.input { callOrElse("git", "rev-parse", "--short=8", "HEAD")("unknown") }
      def gitHasChanges = T.input { callOrElse("git", "diff-index", "--name-only", "HEAD")("dummyChanges").nonEmpty }

      def buildInfoPackageName = "vct.main"
      override def buildInfoMembers = T {
        Seq(
          BuildInfo.Value("name", "VerCors"),
          BuildInfo.Value("version", "2.0.0"),
          BuildInfo.Value("scalaVersion", main.scalaVersion()),
          BuildInfo.Value("sbtVersion", "-"),
          BuildInfo.Value("currentBranch", gitBranch()),
          BuildInfo.Value("currentCommit", gitCommit()),
          BuildInfo.Value("currentShortCommit", gitShortCommit()),
          BuildInfo.Value("gitHasChanges", gitHasChanges().toString),
          BuildInfo.Value("silverCommit", viper.silver.repo.commitish()),
          BuildInfo.Value("siliconCommit", viper.silicon.repo.commitish()),
          BuildInfo.Value("carbonCommit", viper.carbon.repo.commitish()),
        )
      }
    }
  }

  object allTests extends ScalaModule with ReleaseModule {
    def packedResources = T.sources()
    override def moduleDeps: Seq[JavaModule] = Seq(col.test, viperApi.test, main.test)

    override def mainClass = T { Some("org.scalatest.tools.Runner") }

    def test(args: String*) = T.command {
      col.test.test(args: _*)
      viperApi.test.test(args: _*)
      main.test.test(args: _*)
    }
  }
}