package vct.test.integration.helper

import ch.qos.logback.classic.{Level, Logger}
import hre.io.Readable
import org.scalactic.source.Position
import org.scalatest.{Resources, Tag, Transformer}
import org.scalatest.flatspec.{AnyFlatSpec, AnyFlatSpecLike}
import org.slf4j.LoggerFactory
import vct.col.origin.VerificationFailure
import vct.main.Main.TemporarilyUnsupported
import vct.main.modes.Verify
import vct.options
import vct.options.types
import vct.options.types.{Backend, PathOrStd}
import vct.parsers.ParseError
import vct.result.VerificationError
import vct.result.VerificationError.UserError

import java.nio.file.{Path, Paths}

abstract class VercorsSpec extends AnyFlatSpec {
  var coveredExamples: Seq[Path] = Nil

  sealed trait Verdict {
    override def toString: String = this match {
      case Pass => "pass"
      case AnyFail => "fail"
      case Fail(_) => "fail"
      case Error(_) => "error"
    }
  }
  case object Pass extends Verdict
  case object AnyFail extends Verdict
  case class Fail(code: String) extends Verdict
  case class Error(code: String) extends Verdict

  case class IncompleteVerdict(fromCode: String => Verdict)
  case object ErrorVerdict

  private def registerTest(verdict: Verdict, desc: String, tags: Seq[Tag], backend: Backend, inputs: Seq[Readable]): Unit = {
    registerTest(s"${desc.capitalize} should $verdict with $backend", tags: _*) {
      LoggerFactory.getLogger("viper").asInstanceOf[Logger].setLevel(Level.OFF)
      LoggerFactory.getLogger("vct").asInstanceOf[Logger].setLevel(Level.INFO)

      matchVerdict(verdict, backend match {
        case types.Backend.Silicon => Verify.verifyWithSilicon(inputs)
        case types.Backend.Carbon => Verify.verifyWithCarbon(inputs)
      })
    }
  }

  private def matchVerdict(verdict: Verdict, value: Either[VerificationError, Seq[VerificationFailure]]): Unit = {
    value match {
      case Left(err: TemporarilyUnsupported) =>
        println(err)
        cancel()
      case Left(err: ParseError) if err.message.contains("not supported") =>
        println(err)
        cancel()
      case _ =>
    }

    verdict match {
      case Pass => value match {
        case Left(err) =>
          println(err)
          fail("Expected the test to pass, but it returned an error instead.")
        case Right(Nil) => // success
        case Right(fails) =>
          fails.foreach(f => println(f.toString))
          fail("Expected the test to pass, but it returned verification failures instead.")
      }
      case AnyFail => value match {
        case Left(err) =>
          println(err)
          fail("Expected the test to fail, but it returned an error instead.")
        case Right(Nil) =>
          fail("Expected the test to fail, but it passed instead.")
        case Right(_) => // success
      }
      case Fail(code) => value match {
        case Left(err) =>
          println(err)
          fail("Expected the test to fail, but it returned an error instead.")
        case Right(Nil) =>
          fail("Expected the test to fail, but it passed instead.")
        case Right(fails) => fails.filterNot(_.code == code) match {
          case Nil => // success
          case fails =>
            fails.foreach(f => println(f.toString))
            fail(f"Expected the test to fail with error code $code, but got ${fails.map(_.code).mkString(", ")} instead.")
        }
      }
      case Error(code) => value match {
        case Left(err: UserError) if err.code == code => // success
        case Left(err: UserError) =>
          println(err.toString)
          fail(f"Expected the test to error with code $code, but got ${err.code} instead.")
        case Left(err) =>
          println(err.toString)
          fail(f"Expected the test to error with code $code, but got the above error instead.")
        case Right(_) =>
          fail("Expected the test to error, but got a pass or fail instead.")
      }
    }
  }

  class VercorsWord {
    def should(verdict: Verdict): VerdictPhrase = new VerdictPhrase(verdict)
    def should(verdict: IncompleteVerdict): CodeVerdictPhrase = new CodeVerdictPhrase(verdict)
    def should(verdict: ErrorVerdict.type): ErrorVerdictPhrase = new ErrorVerdictPhrase()
  }

  class CodeVerdictPhrase(val verdict: IncompleteVerdict) {
    def withCode(code: String): VerdictPhrase = new VerdictPhrase(verdict.fromCode(code))
  }

  class ErrorVerdictPhrase() {
    def withCode(code: String): BackendPhrase = new BackendPhrase(Error(code), silicon)
  }

  class VerdictPhrase(val verdict: Verdict) {
    def using(backend: Seq[Backend]): BackendPhrase = new BackendPhrase(verdict, backend)
  }

  class BackendPhrase(val verdict: Verdict, val backends: Seq[Backend]) {
    def example(path: String): Unit = examples(path)

    def examples(examples: String*): Unit = {
      val paths = examples.map(ex => Paths.get(s"examples/$ex"))
      coveredExamples ++= paths
      val inputs = paths.map(PathOrStd.Path)

      for(backend <- backends) {
        registerTest(verdict, s"Examples ${paths.mkString(", ")}", Seq(new Tag("exampleCase")), backend, inputs)
      }
    }

    def in(desc: String): DescPhrase = new DescPhrase(verdict, backends, desc)
  }

  class DescPhrase(val verdict: Verdict, val backends: Seq[Backend], val desc: String) {
    def pvl: String => Unit = { data =>
      val inputs = Seq(LiteralReadable("test.pvl", data))
      for(backend <- backends) {
        registerTest(verdict, desc, Seq(new Tag("literalCase")), backend, inputs)
      }
    }

    def java(data: String): Unit = {
      val inputs = Seq(LiteralReadable("test.java", data))
      for(backend <- backends) {
        registerTest(verdict, desc, Seq(new Tag("literalCase")), backend, inputs)
      }
    }

    def c(data: String): Unit = {
      val inputs = Seq(LiteralReadable("test.c", data))
      for(backend <- backends) {
        registerTest(verdict, desc, Seq(new Tag("literalCase")), backend, inputs)
      }
    }
  }

  val vercors: VercorsWord = new VercorsWord
  val verify: Verdict = Pass
  val fail: IncompleteVerdict = IncompleteVerdict(Fail)
  val error: ErrorVerdict.type = ErrorVerdict

  val silicon: Seq[Backend] = Seq(types.Backend.Silicon)
  val carbon: Seq[Backend] = Seq(types.Backend.Carbon)
  val someBackend: Seq[Backend] = Seq(types.Backend.Silicon)
  val anyBackend: Seq[Backend] = Seq(types.Backend.Silicon, types.Backend.Carbon)
}
