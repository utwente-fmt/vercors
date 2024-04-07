package util

import mill.{PathRef, T, Task, pathReadWrite}
import os.Path

trait ReleaseModule extends JavaModule with SeparatePackedResourcesModule {
  def name: T[String] = T { this.getClass.getSimpleName.replace("$", "").capitalize }
  def executableName: T[String] = T { name().toLowerCase }
  override def artifactName: T[String] = T { name().toLowerCase }
  def version: T[String] = T { "0.1-SNAPSHOT" }
  def dockerName: T[DockerImageName] = T { DockerImageName.Plain(name().toLowerCase) }
  def dockerVersion: T[Option[String]] = T { None }
  def dockerTag: T[String] = T {
    dockerVersion() match {
      case None => dockerName().toString
      case Some(tag) => s"${dockerName()}:$tag"
    }
  }
  def maintainer: T[String] = T { "Unknown <unknown@example.com>" }
  def summary: T[String] = T { s"${name()} test build" }
  def description: T[String] = T { s"${name()} test build" }
  def homepage: T[Option[String]] = T { None }

  def debianPackageName: T[String] = T { name().toLowerCase }
  def debianSection: T[String] = T { "java" }
  def debianArchitecture: T[String] = T { "all" }

  def winExecutableName: T[String] = T { executableName() + ".bat" }

  def dockerAptDependencies: T[Seq[String]] = T { Seq.empty[String] }

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
      "docker" -> dockerBuild()._2.path,
    )
  }

  def dockerExtra: T[String] = ""

  def dockerBuild: T[(String, PathRef)] = T {
    val dest = T.dest / "dest"
    val data = dest / "opt" / artifactName()
    val bin = dest / "usr" / "bin"

    os.makeDir.all(data)
    os.makeDir.all(bin)

    val jar = copy(assembly().path, data / s"${executableName()}.jar")
    val res = bareClasspath().map(res => copy(res, data / res.last))

    os.walk(data / "deps" / "win", preOrder = false).foreach(os.remove)
    os.walk(data / "deps" / "darwin", preOrder = false).foreach(os.remove)

    os.write(data / ".classpath", "-cp " + (jar +: res).map(_.relativeTo(dest)).map(os.root / _).map(_.toString).mkString(":"))

    os.write(bin / executableName(),
      s"""#!/bin/sh
         |java ${forkArgs().mkString(" ")} @/opt/${artifactName()}/.classpath ${finalMainClass()} "$$@"
         |""".stripMargin)
    os.perms.set(bin / executableName(), os.PermSet.fromString("rwxrwxr-x"))

    val entryPoint =
      Seq("java") ++ forkArgs() ++ Seq(s"@/.classpath", finalMainClass())

    os.write(T.dest / "Dockerfile",
      s"""FROM ubuntu:jammy
         |RUN apt update && \\
         |    apt install --no-install-recommends -y openjdk-17-jre-headless ${dockerAptDependencies().mkString(" ")}
         |COPY dest /
         |${dockerExtra()}
         |ENTRYPOINT ${executableName()}
         |""".stripMargin)

    os.proc("docker", "build", "--file", T.dest / "Dockerfile", "--iidfile", T.dest / "id", ".").call(cwd = T.dest)
    val id = os.read(T.dest / "id")
    val tempTag = s"mill-build-${id.replace(":", "-")}"
    os.proc("docker", "tag", id, tempTag).call(cwd = T.dest)

    val out = T.dest / s"${executableName()}-${version()}-docker.tar"
    os.proc("docker", "save", "-o", out, id).call(cwd = T.dest)

    // Clean up the image unless it is tagged another way
    os.proc("docker", "rmi", tempTag).call(cwd = T.dest)

    (id, PathRef(out, quick = true))
  }

  def dockerPublishLocal() = T.command {
    val (id, file) = dockerBuild()

    os.proc("docker", "load", "-i", file.path).call(cwd = T.dest)
    os.proc("docker", "tag", id, dockerTag()).call(cwd = T.dest)

    ()
  }

  def dockerTar = T {
    val (id, file) = dockerBuild()

    val tag = dockerTag()

    val exists = os.proc("docker", "image", "inspect", tag).call(cwd = T.dest, check = false).exitCode == 0

    os.proc("docker", "load", "-i", file.path).call(cwd = T.dest)
    os.proc("docker", "tag", id, dockerTag()).call(cwd = T.dest)
    val out = T.dest / s"${executableName()}-${version}-docker.tar"
    os.proc("docker", "save", "-o", out, tag).call(cwd = T.dest)

    if(!exists) {
      os.proc("docker", "rmi", tag).call(cwd = T.dest)
    }

    PathRef(out, quick = true)
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
    val outName = s"${debianPackageName()}-${version()}-debian"
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