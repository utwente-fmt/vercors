package util

import mill.define.Command
import mill.scalalib.scalafmt.{ScalafmtModule, ScalafmtWorkerModule}
import mill.scalalib.{DepSyntax, TestModule}
import mill.{Agg, PathRef, T, pathReadWrite}
import os.{Path, RelPath}

trait VercorsModule extends ScalaModule with ScalafmtModule with VercorsJavaModule { outer =>
  def changedFilesInIndex(): Command[Seq[Path]] = T.command {
    os.proc("git", "diff-index", "--cached", "--name-only", "HEAD")
      .call(cwd = settings.root)
      .out.lines()
      .map(settings.root / RelPath(_))
  }

  def changedFilesInWorkdir(): Command[Seq[Path]] = T.command {
    os.proc("git", "diff-files", "--name-only")
      .call(cwd = settings.root)
      .out.lines()
      .map(settings.root / RelPath(_))
  }

  def stagedFilesToFormat: T[Seq[PathRef]] = T.sources {
    val allFilesToFormat = filesToFormat(sources())

    val changedFilesInIndex = outer.changedFilesInIndex()()
    val changedFilesInWorkdir = outer.changedFilesInWorkdir()()

    val initialFilesToFormat = allFilesToFormat.map(_.path).intersect(changedFilesInIndex)

    val changedInBoth = initialFilesToFormat.intersect(changedFilesInWorkdir)

    if(changedInBoth.nonEmpty) {
      T.log.error("These paths will not be formatted, as they have unstaged changes:")
      for(path <- changedInBoth)
        T.log.error("- " + path.toString())
    }

    initialFilesToFormat.diff(changedFilesInWorkdir).map(PathRef(_))
  }

  def reformatStaged(): Command[Unit] = T.command {
    val targets = stagedFilesToFormat()

    ScalafmtWorkerModule
      .worker()
      .reformat(
        targets,
        scalafmtConfig().head,
      )

    os.proc("git", "add", targets.map(_.path)).call()
    ()
  }

  trait Tests extends ScalaTests with TestModule.ScalaTest with VercorsJavaModule {
    def key = outer.key
    override def sourcesDir = T { settings.test / key }
    override def sources = T.sources { sourcesDir() }
    def deps = T { Agg.empty }
    override def ivyDeps = settings.deps.common ++ Agg(ivy"org.scalatest::scalatest:3.2.7") ++ outer.deps() ++ deps()
  }
}