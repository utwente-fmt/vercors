package vct.test

import java.nio.file.{FileSystems, FileVisitOption, Files, Path, Paths}
import java.util.concurrent.{Executors, Future}

import hre.config.{BooleanSetting, Configuration, IntegerSetting, OptionParser, StringListSetting, StringSetting}
import hre.lang.HREExitException
import hre.util.TestReport.Verdict
import hre.lang.System.Warning
import hre.lang.System.Progress
import hre.lang.System.Output
import hre.lang.System.Abort
import hre.lang.System.Debug

import scala.collection.JavaConverters._
import scala.collection.mutable

sealed trait CaseFilter {
  def addOptions(parser: OptionParser)
  def isPossible(kees: Case): Boolean
}

object IncludeSuite extends CaseFilter {
  private val includes = new StringListSetting()
  private val option = includes.getAppendOption("include test suites")

  override def addOptions(parser: OptionParser): Unit = parser.add(option, "include-suite")

  override def isPossible(kees: Case): Boolean =
    !option.used() || includes.asScala.exists(kees.suites.contains(_))
}

object ExcludeSuite extends CaseFilter {
  private val excludes = new StringListSetting()
  private val option = excludes.getAppendOption("exclude test suites")

  override def addOptions(parser: OptionParser): Unit = parser.add(option, "exclude-suite")

  override def isPossible(kees: Case): Boolean =
    !option.used() || excludes.asScala.forall(!kees.suites.contains(_))
}

object Language extends CaseFilter {
  private val langs = new StringListSetting()
  private val option = langs.getAppendOption("select test input languages")

  override def addOptions(parser: OptionParser): Unit = parser.add(option, "lang")

  override def isPossible(kees: Case): Boolean =
    !option.used() || kees.files.asScala.exists(f => langs.contains(RecursiveFileVisitor.extension(f)))
}

object CommandLineTesting {
  private val caseFilters = Seq(IncludeSuite, ExcludeSuite, Language)

  private val backendFilter = new StringListSetting()
  private val backendFilterOption = backendFilter.getAppendOption("select the backends to run tests for")

  private val builtinTest = new BooleanSetting(false)
  private val builtinTestOption = builtinTest.getEnable("execute the builtin tests")

  private val testDirs = new StringListSetting()
  private val testDirsOption = testDirs.getAppendOption(
    "execute test suites from the command line. " +
    "Each test suite is a folder which is scanned for valid test inputs")

  private val saveDir = new StringSetting(null)
  private val saveDirOption = saveDir.getAssign("save intermediate files to given directory")

  private val workers = new IntegerSetting(1);
  private val workersOption = workers.getAssign("set the number of parallel test workers")

  private val travisTestOutput = new BooleanSetting(false)
  private val travisTestOutputOption = travisTestOutput.getEnable("output the full output of failing test cases as a foldable section in travis")

  // The tools are marked lazy, so they are only loaded when in use by at least one example. Erroring out on missing
  // dependencies that we don't use would be silly.
  private lazy val z3 = Configuration.getZ3
  private lazy val boogie = Configuration.getBoogie
  private lazy val chalice = Configuration.getChalice
  private lazy val dafny = Configuration.getDafny
  private lazy val carbon = Configuration.getCarbon
  private lazy val silicon = Configuration.getSilicon
  private lazy val vercors = Configuration.getThisVerCors(null)

  private def selfTest(name: String): String =
    Configuration.getSelfTestPath(name).getAbsolutePath

  private val builtinTests = Map(
    "!z3-sat" -> Task(z3.withArgs("-smt2", selfTest("test-sat.smt")), Seq(
      MustSay("p true"),
      MustSay("q true"),
    )),
    "!z3-unsat" -> Task(z3.withArgs("-smt2", selfTest("test-unsat.smt")), Seq(
      MustSay("unsat"),
    )),
    "!boogie-pass" -> Task(boogie.withArgs(selfTest("test-pass.bpl")), Seq(
      MustSay("Boogie program verifier finished with 1 verified, 0 errors")
    )),
    "!boogie-fail" -> Task(boogie.withArgs(selfTest("test-fail.bpl")), Seq(
      MustSay("Boogie program verifier finished with 0 verified, 1 error")
    )),
    "!chalice-pass" -> Task(chalice.withArgs(selfTest("test-pass.chalice")), Seq(
      MustSay("Boogie program verifier finished with 3 verified, 0 errors")
    )),
    "!chalice-fail" -> Task(chalice.withArgs(selfTest("test-fail.chalice")), Seq(
      MustSay("Boogie program verifier finished with 2 verified, 1 error")
    )),
    "!dafny-pass" -> Task(dafny.withArgs("/compile:0", selfTest("test-pass.dfy")), Seq(
      MustSay("Dafny program verifier finished with 2 verified, 0 errors")
    )),
    "!dafny-fail" -> Task(dafny.withArgs("/compile:0", selfTest("test-fail.dfy")), Seq(
      MustSay("Dafny program verifier finished with 1 verified, 1 error"),
      ExpectVerdict(Verdict.Error)
    )),
    "!carbon-pass" -> Task(carbon.withArgs(selfTest("test-pass.sil")), Seq(
      MustSay("No errors found.")
    )),
    "!carbon-fail" -> Task(carbon.withArgs(selfTest("test-fail.sil")), Seq(
      MustSay("Assignment might fail. Divisor 0 might be zero."),
      ExpectVerdict(Verdict.Error)
    )),
    "!silicon-pass" -> Task(silicon.withArgs(selfTest("test-pass.sil")), Seq(
      MustSay("No errors found.")
    )),
    "!silicon-fail" -> Task(silicon.withArgs(selfTest("test-fail.sil")), Seq(
      MustSay("Assignment might fail. Divisor 0 might be zero."),
      ExpectVerdict(Verdict.Error)
    )),
  )

  def addOptions(parser: OptionParser): Unit = {
    caseFilters.foreach(_.addOptions(parser))
    parser.add(backendFilterOption, "tool")
    parser.add(testDirsOption, "test")
    parser.add(builtinTestOption, "test-builtin")
    parser.add(saveDirOption, "save-intermediate")
    parser.add(workersOption, "test-workers")
    parser.add(travisTestOutputOption, "travis-test-output")
  }

  def getCases: Map[String, Case] = {
    val visitor = new RecursiveFileVisitor
    testDirs.forEach(dir =>
      Files.walkFileTree(Paths.get(dir), Set(FileVisitOption.FOLLOW_LINKS).asJava, Integer.MAX_VALUE, visitor))

    var will_fail = visitor.delayed_fail

    if (!visitor.unmarked.isEmpty) {
      Warning("There are unmarked files:")
      visitor.unmarked.forEach(path => Warning("%s", path))
      will_fail = true
    }

    if (will_fail) {
      Output("Because of warnings above, the test suite will not run.")
      throw new HREExitException(1)
    }

    visitor.testsuite.asScala.filter({case (_, kees) => caseFilters.forall(_.isPossible(kees))}).toMap
  }

  def getTasks: Map[String, Task] = {
    var result = mutable.HashMap[String, Task]()

    if(builtinTest.get()) {
      result ++= builtinTests
    }

    // Ensure jacoco output dir exists
    // TODO: Weird place for this
    val jacocoOutputDir = Paths.get("jacoco_output").toFile
    if (jacocoOutputDir.exists()) {
      Abort("jacoco_output dir already exists")
    }
    jacocoOutputDir.mkdir

    for ((name, kees) <- getCases) {
      for (tool <- kees.tools.asScala) {
        if (!backendFilterOption.used() || backendFilter.contains(tool)) {
          var args = mutable.ArrayBuffer[String]()
          args += "--progress"
          args += "--" + tool
          args ++= kees.options.asScala
          args ++= kees.files.asScala.map(_.toAbsolutePath.toString)

          var conditions = mutable.ArrayBuffer[TaskCondition]()
          if (kees.verdict != null) {
            conditions += ExpectVerdict(kees.verdict)
          } else {
            conditions += ExpectVerdict(Verdict.Pass)
          }
          if (kees.pass_non_fail) {
            conditions += PassNonFail(kees.fail_methods.asScala.toSeq)
          }
          conditions ++= kees.pass_methods.asScala.map(name => PassMethod(name))
          conditions ++= kees.fail_methods.asScala.map(name => FailMethod(name))

          val jacocoOutputFilePath = s"${jacocoOutputDir.getAbsolutePath}/jacoco_case_${tool}_${name}.exec"
          val jacocoArg = Array(s"-javaagent:${Configuration.getJacocoAgentPath()}=destfile=$jacocoOutputFilePath")
          val vercorsProcess = Configuration.getThisVerCors(jacocoArg).withArgs(args:_*)

          result += ("case-" + tool + "-" + name -> Task(vercorsProcess, conditions))
        }
      }
    }

    result.toMap
  }

  def generateJacocoXML(): Unit = {
    val jacocoCli = Configuration.getJacocoCli
    jacocoCli.addArg("report")

    // Add all coverage files
    val jacocoOutputDir = Paths.get("jacoco_output")
    Files.list(jacocoOutputDir).forEach((execFilePath) => {
      if (!execFilePath.endsWith(".exec"))
        jacocoCli.addArg(execFilePath.toAbsolutePath().toString)
      Debug("Adding: %s", execFilePath.toAbsolutePath().toString)
    })

    // Indicate class path
    System.getProperty("java.class.path").split(':').foreach((cp: String) => {
      // TODO: This breaks if you put vercors in directory not called vercors? Or if classfiles are stored in a weird place? I just want the classpath of vercors and its subprojects (col, hre, parser)
      // TODO: Would rather put the entire classpath here. But one of our classes is called "ImplicitConversions", which clases with a Scala class. This is problematic for jacoco.
      if (cp.contains("vercors")) {
        jacocoCli.addArg("--classfiles", cp)
      }
    })

    jacocoCli.addArg("--xml", Paths.get("jacoco.xml").toFile.getAbsolutePath)

    Output("Aggegrating coverages...")
    val task = Task(jacocoCli, Seq())
    task.call

    // Clean up coverage files
    Files.list(jacocoOutputDir).forEach((execFilePath) => {
      execFilePath.toFile.delete
    })
    jacocoOutputDir.toFile.delete
  }

  def runTests(): Unit = {
    val tasks = getTasks
    val sortedTaskKeys = tasks.keys.toSeq.sorted

    var splitDiv = 1
    var splitMod = 0

    if (System.getenv("SPLIT") != null) {
      val parts = System.getenv("SPLIT").split("/")
      if (parts.length != 2) {
        Warning("%s", "SPLIT environment variable in incorrect format, will ignore and run all tests.")
      }
      try {
        val tmp0 = Integer.parseInt(parts(0))
        val tmp1 = Integer.parseInt(parts(1))
        splitMod = tmp0
        splitDiv = tmp1
      } catch {
        case _: NumberFormatException =>
          Warning("%s", "SPLIT environment variable in incorrect format, will ignore and run all tests.")
      }
    }

    val taskKeys = (splitMod to sortedTaskKeys.length by splitDiv).collect(sortedTaskKeys)
    val pool = Executors.newFixedThreadPool(workers.get())
    var futures = mutable.ArrayBuffer[(Future[Seq[FailReason]], String)]()

    Progress("Submitting %d tasks to thread pool with %d worker(s)", Int.box(taskKeys.length), Int.box(workers.get()))

    for (taskKey <- taskKeys) {
      val task = tasks(taskKey)
      val future = pool.submit(task)
      futures += ((future, taskKey))
    }

    var fails = 0

    for (((future, taskKey), i) <- futures.zipWithIndex) {
      val reasons = future.get()
      val progress = 100 * i / futures.size

      if (reasons.isEmpty) {
        Progress("[%02d%%] Pass: %s", Int.box(progress), taskKey)
      } else {
        fails += 1
        Progress("[%02d%%] Fail: %s", Int.box(progress), taskKey)

        if(travisTestOutput.get()) {
          Output("%s", "travis_fold:start:case_output\r\u001b[0KOutput from case...");

          for(msg <- tasks(taskKey).log) {
            Output(msg.getFormat, msg.getArgs:_*)
          }

          Output("travis_fold:end:case_output");
        }

        reasons.foreach {
          case NullMessage =>
            Output("- Received a null message (internal error?)")
          case InternalError(description) =>
            Output("- Internal error: %s", description)
          case MissingVerdict =>
            Output("- There was no verdict")
          case InconsistentVerdict(older, newer) =>
            Output("- Inconsistent verdict: earlier verdict was %s, new veridct is %s", older, newer)
          case WrongVerdict(expect, got) =>
            Output("- Wrong verdict: expected %s, got %s", expect, got)
          case MethodPass(name) =>
            Output("- Method verdict of method %s was pass, but expected fail", name)
          case MethodFail(name) =>
            Output("- Method verdict of method %s was fail, but expected pass", name)
          case DoesNotSay(text) =>
            Output("- Output did not contain '%s'", text)
        }
      }
    }

    Output("Verification times:")

    for (taskKey <- taskKeys) {
      val time = tasks(taskKey).times.get("entire run")
      Output("%-40s: %s", taskKey, time match {
        case None => "unknown"
        case Some(ms) => String.format("%dms", Int.box(ms))
      })
    }

    generateJacocoXML

    if (fails > 0) {
      hre.lang.System.Verdict("%d out of %d run tests failed", Int.box(fails), Int.box(futures.length))
      System.exit(1)
    } else {
      hre.lang.System.Verdict("All %d tests passed", Int.box(futures.length))
      System.exit(0)
    }
  }

  def enabled: Boolean = testDirsOption.used() || builtinTestOption.used()
}
