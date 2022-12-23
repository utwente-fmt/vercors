package vct.test.integration.helper

import java.io.File
import java.nio.file.Paths

case object ExampleFiles {
  val IGNORE_DIRS: Seq[String] = Seq(
    "examples/private/",
    "examples/archive/",
    "examples/technical/veymont-check/",
  ).map(_.replaceAll("/", File.separator))

  val IGNORE_EXTS: Seq[String] = Seq(
    ".h",
    ".bib",
  )

  val IGNORE_FILES: Set[String] = Set(
    ".gitignore",
    "package-info.java",
    "Makefile",
    "README",
  )

  val MAIN_FILES: Set[String] = Set(
    "examples/concepts/forkjoin/TestOwickiGries.java",
    "examples/concepts/waitnotify/Test.java",
    "examples/concepts/openmp/test-main.c",
    "examples/concepts/openmp/test-other.c",
  ).map(_.replaceAll("/", File.separator))

  val EXCLUSIONS: Seq[File => Boolean] = Seq(
    f => IGNORE_DIRS.exists(dir => f.toString.startsWith(dir)),
    f => MAIN_FILES.contains(f.toString),
    f => IGNORE_FILES.contains(f.getName),
    f => IGNORE_EXTS.exists(ext => f.getName.endsWith(ext)),
  )

  val FILES: Seq[File] = find(Paths.get("examples").toFile)

  def find(directory: File): Seq[File] =
    Option(directory.listFiles()) match {
      case Some(files) => files.filterNot(f => EXCLUSIONS.exists(_(f))).sortBy(_.getName).flatMap(f => if(f.isDirectory) find(f) else Seq(f))
      case None => Nil
    }
}
