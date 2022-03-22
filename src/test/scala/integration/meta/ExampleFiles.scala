package integration.meta

import java.io.File
import java.nio.file.Paths

case object ExampleFiles {
  val EXCLUSIONS: Seq[File => Boolean] = Seq(
    _.getName == ".gitignore",
  )

  val FILES: Seq[File] = find(Paths.get("examples").toFile)

  def find(directory: File): Seq[File] =
    Option(directory.listFiles()) match {
      case Some(files) => files.filterNot(f => EXCLUSIONS.exists(_(f))).sortBy(_.getName).flatMap(f => if(f.isDirectory) find(f) else Seq(f))
      case None => Nil
    }
}
