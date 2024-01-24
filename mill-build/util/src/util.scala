import mill._
import scalalib.{JavaModule => BaseJavaModule, ScalaModule => BaseScalaModule, _}
import mill.contrib.scalapblib.{ScalaPBModule => BaseScalaPBModule}
import os.Path

package object util {
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
    override def scalaPBFlatPackage = true
    override def scalaPBSearchDeps = true
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
      (Nil/*localCompileClasspath().toSeq*/) ++ packedResources() ++ Agg(compile().classes)
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
