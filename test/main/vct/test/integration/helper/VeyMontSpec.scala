package vct.test.integration.helper

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import hre.io.LiteralReadable
import hre.util.FilesHelper
import org.scalactic.source
import org.scalatest.Tag
import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.time.{Seconds, Span}
import org.slf4j.LoggerFactory
import vct.col.origin.VerificationFailure
import vct.main.modes.VeyMont.StageResult
import vct.main.modes.VeyMont
import vct.options.types.PathOrStd
import vct.options.Options
import vct.result.VerificationError
import vct.result.VerificationError.{SystemError, UserError}
import vct.test.integration.helper.{ResultModel => R}
import vct.test.integration.helper.VercorsSpec.MATRIX_COUNT

import java.nio.file.{Path, Paths}

sealed trait ResultModel
object ResultModel {
  def ofTestSpec(
      fail: String,
      fails: Seq[String],
      error: String,
  ): ResultModel = {
    assert(!(fail != null && error != null && fails != null))
    (fail, fails, error) match {
      case (null, null, null) => Pass
      case (fail, null, null) => Fail(Seq(fail))
      case (null, fails, null) => Fail(fails)
      case (null, null, error) => Error(error)
      case _ => ???
    }
  }

  def ofFailure(fails: Seq[VerificationFailure]): ResultModel =
    if (fails.isEmpty)
      Pass
    else
      Fail(fails.map(_.code))

  // String output is a user-readable string representation of the verification run. Useful for manual inspection.
  def ofFrontendOutput(
      res: Either[VerificationError, StageResult]
  ): (ResultModel, String) =
    res match {
      case Left(error: UserError) => (Error(error.code), error.text)
      case Left(crash: SystemError) => (Crash(crash), crash.text)
      case Right(stageResult) =>
        stageResult match {
          case VeyMont.ChoreographyResult(_, fails) =>
            (ofFailure(fails), fails.map(_.desc).mkString("\n"))
          case VeyMont.GenerateResult(_) => (Pass, "(no tool output)")
          case VeyMont.ImplementationResult(fails) =>
            (ofFailure(fails), fails.map(_.desc).mkString("\n"))
        }
    }

  // We use strings here because the Fail & Error type needs to be useable as both a test spec (meaning, derivable from
  // a fail/error code that we can write down) and actual test output. If we'd use the actual
  // VerificationFailure/UserError types here, we couldn't specify the expected outputs up front, as that would require
  // actually instantiating these entire objects, which is a lengthy process and requires annoying imports. So we use
  // strings as a handy abstraction that works for both test spec & actual output.
  case class Fail(codes: Seq[String]) extends ResultModel {
    require(codes.nonEmpty)
  }
  case class Error(code: String) extends ResultModel
  // Since a crash is not supposed to happen, we don't put a code or something here, but the actual SystemError
  // that was thrown. Conversely, you cannot write a test that expects a crash, in that case it should either be a
  // verification failure or a user error.
  case class Crash(err: SystemError) extends ResultModel
  case object Pass extends ResultModel
}

class VeyMontSpec extends VercorsSpec with LazyLogging {
  sealed trait Language
  case object Pvl extends Language
  case object Java extends Language

  def choreography(
      pvl: String = null,
      input: Path = null,
      inputs: Seq[Path] = null,
      desc: String = null,
      flags: Seq[String] = Seq(),
      flag: String = null,
      fail: String = null,
      fails: Seq[String] = null,
      error: String = null,
      targetLanguage: Language = Pvl,
  )(implicit pos: source.Position): Unit =
    veymontTest(
      pvl = pvl,
      desc = desc,
      input = input,
      inputs = inputs,
      flags = "--choreography" +: flags,
      flag = flag,
      fail = fail,
      fails = fails,
      error = error,
      targetLanguage = targetLanguage,
    )

  def implementation(
      pvl: String = null,
      desc: String = null,
      input: Path = null,
      inputs: Seq[Path] = null,
      flags: Seq[String] = Seq(),
      flag: String = null,
      fail: String = null,
      fails: Seq[String] = null,
      error: String = null,
      targetLanguage: Language = Pvl,
  )(implicit pos: source.Position): Unit =
    veymontTest(
      pvl = pvl,
      desc = desc,
      input = input,
      inputs = inputs,
      flags = Seq("--implementation") ++ flags,
      flag = flag,
      fail = fail,
      fails = fails,
      error = error,
      targetLanguage = targetLanguage,
    )

  def veymontTest(
      pvl: String = null,
      input: Path = null,
      inputs: Seq[Path] = null,
      desc: String = null,
      fail: String = null,
      fails: Seq[String] = null,
      error: String = null,
      flag: String = null,
      flags: Seq[String] = Seq(),
      targetLanguage: Language = Pvl,
      processImplementation: Path => Unit = null,
  )(implicit pos: source.Position): Unit = {
    // First combine both the single and the sequence input.
    // Using them both is a weird use case but supporting it is okay
    val pathInputs = Option(input).toSeq ++ Option(inputs).toSeq.flatten

    // When giving a pvl literal, there should also be a description
    // Otherwise, inputs should be non-null, so a description can be derived if necessary
    require((pvl != null && desc != null) || pathInputs != Seq())
    val descr =
      if (desc == null)
        s"Files ${pathInputs.mkString(",")}"
      else
        desc
    // Also, either there have to be inputs, or a pvl literal, but not both
    require(pathInputs == Seq() ^ (pvl == null))

    val actualInputs =
      (pathInputs, pvl) match {
        case (inputs, null) =>
          require(inputs.nonEmpty)
          // Register examples in the test suite
          val absoluteExamplePath = Paths.get("examples").toAbsolutePath
          inputs.foreach { p =>
            if (p.toAbsolutePath.startsWith(absoluteExamplePath)) {
              coveredExamples ++= Seq(p)
            }
          }
          inputs.map(PathOrStd.Path)
        case (Seq(), pvl) => Seq(LiteralReadable("test.pvl", pvl))
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
          targetLanguage match {
            case Pvl => temp.resolve("output.pvl")
            case Java => temp.resolve("output.java")
          }
        }

        val outputFlags =
          if (processImplementation != null) {
            Seq("--veymont-output", tempSource.toString)
          } else
            Seq()

        failAfter(Span(400, Seconds)) {
          val expectedResult = ResultModel.ofTestSpec(fail, fails, error)
          val (actualResult, humanReadableResult) = ResultModel
            .ofFrontendOutput(VeyMont.ofOptions(
              Options.parse(
                (Seq("--veymont") ++ outputFlags ++ flags ++ Option(flag).toSeq)
                  .toArray
              ).get,
              actualInputs,
            ))

          processResult(expectedResult, actualResult, humanReadableResult)

          Option(processImplementation).foreach(f => f(tempSource))
        }
      }
    }
  }

  def processResult(
      expected: ResultModel,
      actual: ResultModel,
      humanReadableResult: String,
  ): Unit = {
    info(s"Expected: $expected")
    info(s"Actual: $actual")

    def show(res: ResultModel): String =
      res match {
        case R.Fail(Seq(code)) => s"fail with code $code"
        case R.Fail(codes) => s"fail with codes: ${codes.mkString(", ")}"
        case R.Error(code) => s"error with code $code"
        case R.Crash(err) => s"crash with message:\n${err.text}"
        case R.Pass => "pass"
      }

    assert(
      expected == actual,
      s"\nExpected test result: ${show(expected)}\nActual test result: ${show(actual)}\n--- Output string representation ---\n$humanReadableResult",
    )
  }
}
