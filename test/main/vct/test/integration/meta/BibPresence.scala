package vct.test.integration.meta

import org.scalatest.flatspec.AnyFlatSpec
import vct.test.integration.helper.ExampleFiles

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala

class BibPresence extends AnyFlatSpec {
  def collectNoBibDirs(p: Path): Seq[Path] = {
    require(Files.isDirectory(p))
    val files = Files.list(p).iterator().asScala.toSeq
    val dirs = files.filter(Files.isDirectory(_))
    if (files.exists(_.endsWith("reference.bib"))) { return Seq() }

    val noBibDirs = dirs.flatMap(collectNoBibDirs)
    // If all child directories do not have a reference.bib, probably the reference.bib should be in p
    if (noBibDirs.size == dirs.size)
      Seq(p)
    else
      noBibDirs
  }

  it should "have a reference.bib for every publication" in {
    val dirs = collectNoBibDirs(ExampleFiles.PUBLICATIONS_DIR)
    if (dirs.nonEmpty) {
      fail(
        s"The test suite requires reference.bib files in the following directories: ${dirs.mkString(", ")}"
      )
    }
  }
}
