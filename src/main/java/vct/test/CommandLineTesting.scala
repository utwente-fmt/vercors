package vct.test

import hre.config._
import hre.lang.HREExitException
import hre.lang.System.{Debug, Output, Progress, Warning}
import hre.util.{BackendProcessGenerator, FileHelper, Verdict}
import hre.util.FileHelper.getClassPathElements
import vct.col.features.Feature

import java.io._
import java.nio.file.{FileVisitOption, Files, Paths}
import java.util
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters._

// These need to be included to ensure jacoco is included in the classpath, so it can be used for the test suite.
import org.jacoco.cli
import org.jacoco.agent;

sealed trait CaseFilter {
  def addOptions(parser: OptionParser): Unit
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

  override def isPossible(kees: Case): Boolean = {
    !option.used() || excludes.asScala.forall(!kees.suites.contains(_))
  }
}

object Language extends CaseFilter {
  private val langs = new StringListSetting()
  private val option = langs.getAppendOption("select test input languages")

  override def addOptions(parser: OptionParser): Unit = parser.add(option, "lang")

  override def isPossible(kees: Case): Boolean =
    !option.used() || kees.files.asScala.exists(f => langs.contains(RecursiveFileVisitor.extension(f)))
}

object CommandLineTesting {
  val IDEA_RUN_CONFIG_DIR: String = ".idea/runConfigurations"

  private val caseFilters = Seq(IncludeSuite, ExcludeSuite, Language)

  private val backendFilter = new StringListSetting()
  private val backendFilterOption = backendFilter.getAppendOption("select the backends to run tests for")

  private val builtinTest = new BooleanSetting(false)
  private val builtinTestOption = builtinTest.getEnable("execute the builtin tests")

  private val testDirs = new StringListSetting()
  private val testDirsOption = testDirs.getAppendOption(
    "execute test suites from the command line. " +
    "Each test suite is a folder which is scanned for valid test inputs")

  private val workers = new IntegerSetting(1);
  private val workersOption = workers.getAssign("set the number of parallel test workers")

  private val enableCoverage = new BooleanSetting(false)
  private val enableCoverageOption = enableCoverage.getEnable("Enable coverage instrumenting on the test workers")

  private val tempCoverageReportPath = new StringSetting("temp_jacoco_output")
  private val tempCoverageReportPathOption = tempCoverageReportPath.getAssign("Indicate folder where coverage reports of test workers are stored")

  private val coverageReportFile = new StringSetting("jacoco.xml")
  private val coverageReportFileOption = coverageReportFile.getAssign("Path of where to write the coverage report output file, in xml")

  private val coverageHtmlReportFile = new StringSetting(null)
  private val coverageHtmlReportFileOption = coverageHtmlReportFile.getAssign("Creates a new directory containing coverage report in html")

  private val actionsTestOutput = new BooleanSetting(false)
  private val actionsTestOutputOption = actionsTestOutput.getEnable("output the full output of failing test cases as a foldable section in github actions")

  private val testFailFast = new BooleanSetting(false)
  private val testFailFastOption = testFailFast.getEnable("store test failures at the end of the run, after which failing tests will run first on the next run")

  private val testFailIdeaConfigs = new BooleanSetting(false)
  private val testFailIdeaConfigsOption = testFailIdeaConfigs.getEnable("store test failures as run configurations in .idea/runConfigurations")

  // The tools are marked lazy, so they are only loaded when in use by at least one example. Erroring out on missing
  // dependencies that we don't use would be silly.
  private lazy val z3 = BackendProcessGenerator.getZ3
  private lazy val carbon = BackendProcessGenerator.getCarbon
  private lazy val silicon = BackendProcessGenerator.getSilicon
  private lazy val vercors = BackendProcessGenerator.getThisVerCors(null)

  private def selfTest(name: String): String =
    FileHelper.getSelfTestPath(name).getAbsolutePath

  private val builtinTests = Map(
    "!z3-sat" -> Task(z3.withArgs("-smt2", selfTest("test-sat.smt")), Seq(
      MustSay("p true"),
      MustSay("q true"),
    )),
    "!z3-unsat" -> Task(z3.withArgs("-smt2", selfTest("test-unsat.smt")), Seq(
      MustSay("unsat"),
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
    parser.add(workersOption, "test-workers")
    parser.add(enableCoverageOption, "enable-test-coverage")
    parser.add(tempCoverageReportPathOption, "coverage-temp-dir")
    parser.add(coverageReportFileOption, "coverage-output-file")
    parser.add(coverageHtmlReportFileOption, "coverage-html-dir")
    parser.add(testFailFastOption, "test-fail-fast")
    parser.add(testFailIdeaConfigsOption, "test-fail-idea-configs")
    parser.add(actionsTestOutputOption, "actions-test-output")
  }

  def getCases: Map[String, Case] = {
    val visitor = new RecursiveFileVisitor
    testDirs.forEach(dir =>
      Files.walkFileTree(Paths.get(dir), Set(FileVisitOption.FOLLOW_LINKS).asJava, Integer.MAX_VALUE, visitor))

    var will_fail = visitor.delayedFail

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

  def jacocoJavaAgentArgs(tool: String, caseName: String): Seq[String] = {
    val jacocoOutputDir = Paths.get(tempCoverageReportPath.get()).toFile
    val jacocoOutputFilePath = s"${jacocoOutputDir.getAbsolutePath}/jacoco_case_${tool}_$caseName.exec"
    // Options are of format opt1=val1,op2=val2.
    // Only include our own code, exclude generated parser code.
    val options = s"destfile=$jacocoOutputFilePath,includes=vct.*:hre.*:col.*:viper.api.*,excludes=vct.antlr4.generated.*"
    Seq(s"-javaagent:${FileHelper.getJacocoAgentPath()}=$options")
  }

  def filterEnabledBackends(tools: Set[String]): Set[String] =
    tools.filter(!backendFilterOption.used() || backendFilter.contains(_))

  def createTask(name: String, kees: Case, tool: String) = {
    val args = mutable.ArrayBuffer[String]()
    args += "--progress"
    if (tool != "veymont") {
      // Temporary workaround to disable the tool flag when running a veymont test.
      // This should be removed when we add a proper tool mode for veymont
      args += "--" + tool
      // VeyMont was not built with the feature system in mind, so we have to disable it when veymont is executed
      // in the test suite.
      args += "--strict-internal"
    }
    args ++= kees.options.asScala
    args ++= kees.files.asScala.map(_.toAbsolutePath.toString)

    val conditions = mutable.ArrayBuffer[TaskCondition]()
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

    // Tests are instrumented at runtime by the jacoco java vm agent
    val jacocoArg = if (enableCoverage.get()) { jacocoJavaAgentArgs(tool, name) } else { Seq() }

    val vercorsProcess = BackendProcessGenerator.getThisVerCors(jacocoArg.asJava).withArgs(args.toSeq: _*)

    Task(vercorsProcess, conditions.toSeq)
  }

  def getTasks: Map[String, Task] = {
    val result = mutable.HashMap[String, Task]()

    if(builtinTest.get()) {
      result ++= builtinTests
    }
    for ((name, kees) <- getCases) {
      for (tool <- filterEnabledBackends(kees.tools.asScala.toSet)) {
        result += (s"$name-$tool" -> createTask(name, kees, tool))
      }
    }

    result.toMap
  }

  /**
    * Aggregates several jacoco ".exec" files into one xml report.
    */
  def generateJacocoXML(): Unit = {
    val jacocoCli = BackendProcessGenerator.getJacocoCli
    jacocoCli.addArg("report")

    // Add all coverage files
    val jacocoOutputDir = Paths.get(tempCoverageReportPath.get())
    Files.list(jacocoOutputDir).forEach((execFilePath) => {
      if (!execFilePath.endsWith(".exec"))
        jacocoCli.addArg(execFilePath.toAbsolutePath().toString)
      Debug("Adding: %s", execFilePath.toAbsolutePath().toString)
    })

    // Indicate class path
    // There used to be a problem where jacoco couldn't handly duplicate class names, so we had to filter out some
    // class path elements. But that seems to be fixed, so now we just use the full classpath.
    getClassPathElements().asScala.foreach(jacocoCli.addArg("--classfiles", _))

    jacocoCli.addArg("--xml", Paths.get(coverageReportFile.get()).toFile.getAbsolutePath)
    if (coverageHtmlReportFile.used && coverageHtmlReportFile.get() != null) {
      jacocoCli.addArg("--html", Paths.get(coverageHtmlReportFile.get()).toFile.getAbsolutePath)
    }

    Output("Aggregating coverages...")
    val task = Task(jacocoCli, Seq())
    task.call
    Output("Jacoco tool output")
    for (msg <- task.log) {
      Output(msg.getFormat, msg.getArgs:_*)
    }

    removeTempJacocoDir()
  }

  def removeTempJacocoDir(): Unit = {
    // Clean up coverage files
    val jacocoOutputDir = Paths.get(tempCoverageReportPath.get())
    Files.list(jacocoOutputDir).forEach((execFilePath) => {
      execFilePath.toFile.delete
    })
    jacocoOutputDir.toFile.delete
  }

  def runTests(): Unit = {
    if (enableCoverage.get) {
      // Ensure jacoco output dir exists and remove old reports
      val jacocoOutputDir = Paths.get(tempCoverageReportPath.get()).toFile
      if (jacocoOutputDir.exists) {
        removeTempJacocoDir()
      }
      jacocoOutputDir.mkdir
    }

    val allTasks = getTasks

    if(testFailIdeaConfigs.get()) {
      new File(IDEA_RUN_CONFIG_DIR).mkdir()
    }

    val sortedTaskKeys =
      if(testFailFast.get())
        try {
          val f = Source.fromFile("tmp/failing-tests")
          val failingTests = f.mkString.split(';').filter(_ != "") // silly java split
          f.close()
          allTasks.keys.toSeq.sortBy(key => (!failingTests.contains(key), key))
        } catch {
          case _: FileNotFoundException =>
            allTasks.keys.toSeq.sorted
        }
      else {
        allTasks.keys.toSeq.sorted
      }

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
    val tasks = taskKeys.map(allTasks(_))
    val keyOfTask = allTasks.map{ case (a, b) => (b, a) }.toMap
    val pool = ThreadPool[Task, Seq[FailReason]](workers.get(), tasks)
    Progress("Submitting %d tasks to thread pool with %d worker(s)", Int.box(taskKeys.length), Int.box(workers.get()))
    pool.start()

    var fails = Set.empty[String]
    val intro = mutable.Map[String, Seq[String]]() ++ Feature.ALL.map(f => (f.toString, Seq())).toMap

    for(((task, reasons, newCurrentlyRunning, otherTasksLeft), i) <- pool.results().zipWithIndex) {
      val taskKey = keyOfTask(task)
      val progress = 100 * i / tasks.size

      allTasks(taskKey).log.foreach {
        case msg if msg.getFormat == "stdout: %s" => msg.getArg(0) match {
          case line: String if line.startsWith("!intro") =>
            val parts = line.split(' ')
            if(parts.length == 3)
              parts(2).split(',').foreach(feature => {
                intro.update(feature, intro(feature) :+ parts(1))
              })
          case _ =>
        }
        case _ =>
      }

      for(msg <- task.log) {
        val text = String.format(msg.getFormat, msg.getArgs:_*)
        if(text.contains("[warning]")) {
          Warning("%s: %s", taskKey, text)
        }
      }

      if(testFailIdeaConfigs.get) {
        new File(IDEA_RUN_CONFIG_DIR, s"$taskKey.xml").delete()
      }

      if (reasons.nonEmpty) {
        fails += taskKey
        Output("Fail: %s", taskKey)

        if(testFailIdeaConfigs.get) {
          val dropArgs = BackendProcessGenerator.getThisVerCors(null).getArgs.size
          val args = task.env.getArgs.asScala.drop(dropArgs) ++ Seq("--encoded", "tmp/output.sil")
          val config =
            <component name="ProjectRunConfigurationManager">
              <configuration default="false" name={s"{failing} $taskKey"} type="Application" factoryName="Application">
                <option name="INCLUDE_PROVIDED_SCOPE" value="true" />
                <option name="MAIN_CLASS_NAME" value="vct.main.Main" />
                <module name="vercors" />
                <option name="PROGRAM_PARAMETERS" value={args.mkString(" ")} />
                <option name="VM_PARAMETERS" value="-Xss128M" />
                <method v="2">
                  <option name="Make" enabled="true" />
                </method>
              </configuration>
            </component>

          val f = new File(IDEA_RUN_CONFIG_DIR, s"$taskKey.xml")
          val writer = new OutputStreamWriter(new FileOutputStream(f))
          writer.write(config.toString())
          writer.close()
        }

        if(actionsTestOutput.get()) {
          Output("::group::Case output")

          for(msg <- allTasks(taskKey).log) {
            Output(msg.getFormat, msg.getArgs:_*)
          }

          Output("::endgroup::")
        }

        reasons.foreach {
          case NullMessage =>
            Output("- Received a null message (internal error?)")
          case InternalError(description) =>
            Output("- Internal error: %s", description)
          case ProcessKilled =>
            Output("- Test process was forcibly terminated because it timed out")
          case MissingVerdict =>
            Output("- There was no verdict")
          case InconsistentVerdict(older, newer) =>
            Output("- Inconsistent verdict: earlier verdict was %s, new verdict is %s", older, newer)
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

      Progress("[%02d%%] Running: %s and %d further tasks queued", Int.box(progress), newCurrentlyRunning.map(keyOfTask).mkString(", "), Int.box(otherTasksLeft))
    }

    Output("Verification times:")

    for (taskKey <- taskKeys) {
      val task = allTasks(taskKey)
      val time = task.times.get("entire run")
      Output("%-40s: %s", taskKey, time match {
        case None => "unknown"
        case Some(ms) => String.format("%dms", Int.box(ms))
      })
    }

    if (enableCoverage.get) {
      generateJacocoXML()
    }

    if(testFailFast.get()) {
      val f = new File("tmp/failing-tests")
      val writer = new BufferedWriter(new FileWriter(f))
      writer.write(fails.mkString(";"))
      writer.close()
    }

    if (fails.nonEmpty) {
      hre.lang.System.Verdict("%d out of %d run tests failed", Int.box(fails.size), Int.box(tasks.size))
      for(fail <- fails){
        Output(fail);
      }
      throw new HREExitException(1)
    } else {
      hre.lang.System.Verdict("All %d tests passed", Int.box(tasks.size))
      throw new HREExitException(0)
    }
  }

  def enabled: Boolean = testDirsOption.used() || builtinTestOption.used()
}
