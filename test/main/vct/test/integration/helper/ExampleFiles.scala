package vct.test.integration.helper

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters._

case object ExampleFiles {
  val IGNORE_DIRS: Seq[String] = Seq(
    "examples/private/",
    "examples/archive/",
    "examples/concepts/resourceValues",
    "examples/technical/veymont",
    "examples/concepts/veymont",
    "examples/publications/2023/VeyMontToolPaper",
  ).map(_.replaceAll("/", File.separator))

  val IGNORE_EXTS: Seq[String] = Seq(".h", ".bib", ".xml")

  val IGNORE_FILES: Set[String] = Set(
    ".gitignore",
    "package-info.java",
    "Makefile",
    "README",
    "LICENSE.txt",
    "veymont-tictactoemn-seq.pvl",
  )

  val MAIN_FILES: Set[String] = Set(
    "examples/concepts/forkjoin/TestOwickiGries.java",
    "examples/concepts/waitnotify/Test.java",
    "examples/concepts/openmp/test-main.c",
    "examples/concepts/openmp/test-other.c",
  ).map(_.replaceAll("/", File.separator))

  val EXCLUSIONS: Seq[Path => Boolean] = Seq(
    f => IGNORE_DIRS.exists(dir => f.toString.startsWith(dir)),
    f => MAIN_FILES.contains(f.toString),
    f => IGNORE_FILES.contains(f.getFileName.toString),
    f => IGNORE_EXTS.exists(ext => f.getFileName.toString.endsWith(ext)),
  )

  val FILES: Seq[Path] = find(Paths.get("examples"))

  val PUBLICATIONS_DIR: Path = Paths.get("examples", "publications")

  def find(directory: Path): Seq[Path] =
    Files.list(directory).toScala(Seq).filterNot(f => EXCLUSIONS.exists(_(f)))
      .sortBy(_.getFileName.toString).flatMap(f =>
        if (Files.isDirectory(f))
          find(f)
        else
          Seq(f)
      )
}
