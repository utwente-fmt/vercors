package vct.test

import java.util.concurrent.Callable
import java.util.regex.Pattern
import hre.io.{Message, MessageProcessEnvironment}
import hre.lang.System.Output
import hre.util.TestReport.Verdict

import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class Task(env: MessageProcessEnvironment, conditions: Seq[TaskCondition]) extends Callable[Seq[FailReason]] {
  val TIME_PATTERN: Pattern = Pattern.compile("^(\\s*\\[[^\\]]*\\])*\\s*([a-zA-Z_ ]+) took\\s*([0-9]+)\\s*ms$")

  var log: mutable.Buffer[Message] = mutable.ArrayBuffer()
  var times: mutable.Map[String, Int] = mutable.HashMap()

  var exitCode: Option[Int] = None
  var verdict: Verdict = Verdict.Inconclusive
  var pass_methods: mutable.Set[String] = mutable.HashSet()
  var fail_methods: mutable.Set[String] = mutable.HashSet()

  override def call(): Seq[FailReason] = {
    val p = env.startProcess()

    while (exitCode.isEmpty) {
      val msg = p.recv()
      if (msg == null) {
        return Seq(NullMessage)
      }

      log += msg

      msg.getFormat match {
        case "exit %d" =>
          exitCode = Some(msg.getArg(0).asInstanceOf[Int])
          if (exitCode.get != 0) {
            verdict = Verdict.Error
          }
        case "killed" =>
          return Seq(ProcessKilled)
        case "exec error %s" =>
          return Seq(InternalError(msg.getArg(0).asInstanceOf[String]))
        case "stdout: %s" | "stderr: %s" =>
          val line = msg.getArg(0).asInstanceOf[String]

          val lineMatcher = TIME_PATTERN.matcher(line)
          if(lineMatcher.find()) {
            times.put(lineMatcher.group(2), Integer.parseInt(lineMatcher.group(3)))
          }

          if(line.contains("The final verdict is Pass")) {
            verdict = verdict match {
              case Verdict.Inconclusive | Verdict.Pass =>
                Verdict.Pass
              case other => return Seq(InconsistentVerdict(other, Verdict.Pass))
            }
          }

          if(line.contains("The final verdict is Fail")) {
            verdict = verdict match {
              case Verdict.Inconclusive | Verdict.Fail =>
                Verdict.Fail
              case other => return Seq(InconsistentVerdict(other, Verdict.Fail))
            }
          }

          if(line.contains("The final verdict is Error")) {
            verdict = verdict match {
              case Verdict.Inconclusive | Verdict.Error =>
                Verdict.Error
              case other => return Seq(InconsistentVerdict(other, Verdict.Error))
            }
          }

          if(line.startsWith("method verdict")) {
            val parts = line.split(" ")
            parts(3) match {
              case "PASS" =>
                pass_methods += parts(2)
              case "FAIL" =>
                fail_methods += parts(2)
            }
          }
      }
    }

    conditions.flatMap(_.check(this))
  }
}

sealed trait TaskCondition {
  def check(t: Task): Seq[FailReason]
}

case class MustSay(line: String) extends TaskCondition {
  override def check(t: Task): Seq[FailReason] = {
    if (t.log.exists(msg => (
      (msg.getFormat.equals("stdout: %s") || msg.getFormat.equals("stderr: %s"))
      && msg.getArg(0).asInstanceOf[String].contains(line)
    ))) {
      Seq()
    } else {
      Seq(DoesNotSay(line))
    }
  }
}

case class ExpectVerdict(verdict: Verdict) extends TaskCondition {
  override def check(t: Task): Seq[FailReason] =
    if (t.verdict == verdict) {
      Seq()
    } else {
      Seq(WrongVerdict(verdict, t.verdict))
    }
}

case class PassMethod(method: String) extends TaskCondition {
  override def check(t: Task): Seq[FailReason] =
    if (t.pass_methods.contains(method)) {
      Seq()
    } else {
      Seq(MethodFail(method))
    }
}

case class FailMethod(method: String) extends TaskCondition {
  override def check(t: Task): Seq[FailReason] =
    if (t.fail_methods.contains(method)) {
      Seq()
    } else {
      Seq(MethodPass(method))
    }
}

case class PassNonFail(fail_methods: Seq[String]) extends TaskCondition {
  override def check(t: Task): Seq[FailReason] =
    t.fail_methods.filter(!fail_methods.contains(_)).map(MethodFail).toSeq
}

sealed trait FailReason
object NullMessage extends FailReason
object ProcessKilled extends FailReason
case class InternalError(description: String) extends FailReason
object MissingVerdict extends FailReason
case class InconsistentVerdict(olderVerdict: Verdict, newerVerdict: Verdict) extends FailReason
case class WrongVerdict(expect: Verdict, got: Verdict) extends FailReason
case class MethodPass(method: String) extends FailReason
case class MethodFail(method: String) extends FailReason
case class DoesNotSay(line: String) extends FailReason