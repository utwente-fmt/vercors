package vct.test.integration.helper

import ch.qos.logback.classic.{Level, Logger}
import hre.util.FilesHelper
import org.scalactic.source
import org.scalatest.Tag
import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.time.{Seconds, Span}
import org.slf4j.LoggerFactory
import vct.main.Main
import vct.main.modes.{Verify, VeyMont}
import vct.options.{Options, types}
import vct.test.integration.helper.VercorsSpec.MATRIX_COUNT

import java.nio.file.{Path, Paths}

class VeyMontSpec extends VercorsSpec {
  sealed trait Language
  case object Pvl extends Language
  case object Java extends Language

  def choreography(
      inputs: Seq[Path],
      desc: String = null,
      flags: Seq[String] = Seq(),
  )(implicit pos: source.Position): Unit =
    veymontTest(desc = desc, inputs = inputs, flags = "--choreography" +: flags)

  def implementation(
      desc: String,
      inputs: Seq[Path],
      flags: Seq[String] = Seq(),
  )(implicit pos: source.Position): Unit =
    veymontTest(
      desc = desc,
      inputs = inputs,
      flags = Seq("--implementation") ++ flags,
    )

  def veymontTest(
      inputs: Seq[Path],
      desc: String = null,
      flags: Seq[String] = Seq(),
      language: Language = Pvl,
      processImplementation: Path => Unit = null,
  )(implicit pos: source.Position): Unit = {
    val descr =
      if (desc == null)
        s"Files ${inputs.mkString(",")}"
      else
        desc

    val absoluteExamplePath = Paths.get("examples").toAbsolutePath
    inputs.foreach { p =>
      if (p.toAbsolutePath.startsWith(absoluteExamplePath)) {
        coveredExamples ++= Seq(p)
      }
    }

    val fullDesc: String = s"${descr.capitalize} verifies with silicon"
    // PB: note that object typically do not have a deterministic hashCode, but Strings do.
    val matrixId = Math.floorMod(fullDesc.hashCode, MATRIX_COUNT)
    val matrixTag = Tag(s"MATRIX[$matrixId]")

    registerTest(fullDesc, Tag("MATRIX"), matrixTag) {
      LoggerFactory.getLogger("viper").asInstanceOf[Logger].setLevel(Level.OFF)
      LoggerFactory.getLogger("vct").asInstanceOf[Logger].setLevel(Level.INFO)

      FilesHelper.withTempDir { temp =>
        val tempSource = {
          language match {
            case Pvl => temp.resolve("output.pvl")
            case Java => temp.resolve("output.java")
          }
        }

        val outputFlags =
          if (processImplementation != null) {
            Seq("--veymont-output", tempSource.toString)
          } else
            Seq()

        failAfter(Span(300, Seconds)) {
          VeyMont.verifyGenerateOptions(
            Options.parse(
              (inputs.map(_.toString) ++ Seq("--veymont") ++ outputFlags ++
                flags).toArray
            ).get
          ) match {
            case Left(value) =>
              fail(
                s"Expected test to pass, but finished with the following failure/error: $value"
              )
            case Right(_) =>
          }

          Option(processImplementation).foreach(f => f(tempSource))
        }
      }
    }
  }
}
