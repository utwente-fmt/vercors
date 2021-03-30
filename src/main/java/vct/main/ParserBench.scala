package vct.main

import hre.config.{BooleanSetting, Configuration, OptionParser}
import hre.lang.HREExitException
import hre.lang.System.{Output, Warning}
import vct.test.CommandLineTesting.{caseFilters, testDirs, vercors}
import vct.test.{CommandLineTesting, RecursiveFileVisitor, Task}

import java.nio.file.{FileVisitOption, Files, Path, Paths}
import java.time.{Duration, Instant}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object ParserBench {
  def main(args: Array[String]): Unit = System.exit(new ParserBench().run(args))
}

class ParserBench {
  private val separateRuns = new BooleanSetting(false)
  private val combinedRuns = new BooleanSetting(false)

  private lazy val vercors = Configuration.getThisVerCors

  def run(args: Array[String]): Int = {
    hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info)
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info)

    val clops = new OptionParser
    clops.add(separateRuns.getEnable(""), "separate")
    clops.add(combinedRuns.getEnable(""), "combined")

    clops.parse(args)

    val dir = "examples"
    val visitor = new RecursiveFileVisitor
    Files.walkFileTree(Paths.get(dir), Set(FileVisitOption.FOLLOW_LINKS).asJava, Integer.MAX_VALUE, visitor)
    var will_fail = visitor.delayed_fail
    if (will_fail) {
      Output("Because of warnings above, the test suite will not run.")
      throw new HREExitException(1)
    }

    val cases = visitor.testsuite.asScala

    if (separateRuns.get()) {
      Output("Separate")

      val m : mutable.Map[String, Duration] = mutable.Map()
      for ((name, kees) <- cases) {
        val d = parse(kees.files.asScala.toSeq.map(_.toAbsolutePath.toString).filter(_.contains(".java")))
        Output("%s Took: %dms", name, d.toMillis)
        m.put(name, d)
      }

      Output("name,num_files,bytes,separate_run_millis")
      for ((name, d) <- m) {
        Output("%s,%d,%d,%d",
          name,
          cases.get(name).get.files.size(),
          cases.get(name).get.files.asScala.map(countBytes(_)).sum,
          d.toMillis)
      }

      0
    } else if (combinedRuns.get()) {
      Output("Combined")

      val files = cases.flatMap(_._2.files.asScala.toSeq
        .map(_.toAbsolutePath.toString)
      ).toSeq
      Output("%s", files)

      val d = parse(files)
      Output("Parsing all took: %dms", d.toMillis)

      0
    } else {
      Output("No mode specified")
      1
    }
  }

  def countBytes(file: Path): Int = {
    Files.readAllBytes(file).length
  }

  def parse(files: Seq[String]): Duration = {
    val v = vercors.withArgs(Seq("--parse-only", "--silicon", "--debug", "vct.main.Main") ++ files : _*)
    v.setWorkingDirectory(Paths.get("").toAbsolutePath)
    val t = Task(v, Seq())
    val start = Instant.now()
    t.call()
    val end = Instant.now()
    for (m <- t.log) {
      Output("%s", m.getFormattedMessage)
    }

    Duration.between(start, end)
  }

}
