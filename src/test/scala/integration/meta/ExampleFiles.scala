package integration.meta

import java.io.File
import java.nio.file.Paths

case object ExampleFiles {
  val EXCLUSIONS: Seq[File => Boolean] = Seq(
    _.toString.startsWith("examples/archive/"),
    _.toString == "examples/concepts/forkjoin/TestOwickiGries.java",
    _.getName == ".gitignore",
    _.getName == "package-info.java",
    _.getName == "Makefile",
    _.getName == "README",
    _.getName.endsWith(".h"),
  )

  val FILES: Seq[File] = find(Paths.get("examples").toFile)

  def find(directory: File): Seq[File] =
    Option(directory.listFiles()) match {
      case Some(files) => files.filterNot(f => EXCLUSIONS.exists(_(f))).sortBy(_.getName).flatMap(f => if(f.isDirectory) find(f) else Seq(f))
      case None => Nil
    }
}
