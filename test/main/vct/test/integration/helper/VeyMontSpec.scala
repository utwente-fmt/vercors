package vct.test.integration.helper

import ch.qos.logback.classic.{Level, Logger}
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
  def veymontTest(desc: String, inputs: Seq[Path], flags: String*)(
      implicit pos: source.Position
  ): Unit = {
    val absoluteExamplePath = Paths.get("examples").toAbsolutePath
    inputs.foreach { p =>
      if (p.toAbsolutePath.startsWith(absoluteExamplePath)) {
        coveredExamples ++= Seq(p)
      }
    }

    val fullDesc: String = s"${desc.capitalize} verifies with silicon"
    // PB: note that object typically do not have a deterministic hashCode, but Strings do.
    val matrixId = Math.floorMod(fullDesc.hashCode, MATRIX_COUNT)
    val matrixTag = Tag(s"MATRIX[$matrixId]")

    registerTest(fullDesc, Tag("MATRIX"), matrixTag) {
      LoggerFactory.getLogger("viper").asInstanceOf[Logger].setLevel(Level.OFF)
      LoggerFactory.getLogger("vct").asInstanceOf[Logger].setLevel(Level.INFO)

      failAfter(Span(300, Seconds)) {
        VeyMont.verifyGenerateOptions(
          Options.parse((inputs.map(_.toString) ++ flags).toArray).get
        ) match {
          case Left(value) =>
            fail(
              s"Expected test to pass, but finished with the following failure/error: $value"
            )
          case Right(_) =>
        }
      }
    }
  }
}
