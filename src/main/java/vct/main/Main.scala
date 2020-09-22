// -*- tab-width:2 ; indent-tabs-mode:nil -*-
// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.main

import java.io._
import java.time.Instant
import java.util
import java.util.concurrent.LinkedBlockingDeque

import hre.ast.FileOrigin
import hre.config.{BooleanSetting, ChoiceSetting, CollectSetting, Configuration, IntegerSetting, OptionParser, StringListSetting, StringSetting}
import hre.lang.HREExitException
import hre.lang.System._
import hre.tools.TimeKeeper
import vct.col.ast.expr.{Dereference, StandardOperator}
import vct.col.ast.stmt.decl.{ASTSpecial, Method, ProgramUnit, SpecificationFormat}
import vct.col.util.FeatureScanner
import vct.experiments.learn.SpecialCountVisitor
import vct.logging.PassReport
import vct.silver.ErrorDisplayVisitor
import hre.io.ForbiddenPrintStream
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.features.{Feature, RainbowVisitor}
import vct.main.Passes.BY_KEY
import vct.test.CommandLineTesting

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * VerCors Tool main verifier.
  *
  * @author Stefan Blom
  */
object Main {
  var counters = new util.HashMap[String, SpecialCountVisitor]

  def main(args: Array[String]) = System.exit(new Main().run(args))
}

class Main {
  private var report: PassReport = null
  private var tk: TimeKeeper = null

  private val version = new BooleanSetting(false)
  private val help_passes = new BooleanSetting(false)
  private val logLevel = new ChoiceSetting(Array("silent", "abort", "result", "warning", "info", "progress", "debug", "all"), "info")
  private val debugFilters = new CollectSetting
  private val show_before = new StringListSetting
  private val show_after = new StringListSetting
  private val debugBefore = new CollectSetting
  private val debugAfter = new CollectSetting
  private val show_file = new StringSetting(null)

  private val pass_list = new StringListSetting
  private val pass_list_option = pass_list.getAppendOption("add to the custom list of compilation passes")
  private val stop_after = new StringListSetting
  private val strictInternalConditions = new BooleanSetting(false)

  private val boogie = new BooleanSetting(false)
  private val chalice = new BooleanSetting(false)
  private val silver = new StringSetting("silver")
  private val dafny = new BooleanSetting(false)

  private val check_defined = new BooleanSetting(false)
  private val check_axioms = new BooleanSetting(false)
  private val check_history = new BooleanSetting(false)
  private val separate_checks = new BooleanSetting(false)
  private val sequential_spec = new BooleanSetting(false)
  private val explicit_encoding = new BooleanSetting(false)
  private val global_with_field = new BooleanSetting(false)
  private val infer_modifies = new BooleanSetting(false)
  private val no_context = new BooleanSetting(false)
  private val gui_context = new BooleanSetting(false)
  private val sat_check = new BooleanSetting(true)
  private val abruptTerminationViaExceptions = new BooleanSetting(false)
  private val trigger_generation = new IntegerSetting(0)
  private val learn = new BooleanSetting(false)

  private def parseOptions(args: Array[String]) = {
    val clops = new OptionParser
    clops.add(clops.getHelpOption, Char.box('h'), "help")
    clops.add(version.getEnable("Output the current version and exit"), "version")
    clops.add(logLevel.getSetOption("Set the logging level"), "verbosity")
    clops.add(logLevel.getExplicitOption("progress", "Show progress through the passes"), "progress", Char.box('v'))
    clops.add(logLevel.getExplicitOption("silent", "Never output anything"), "silent", Char.box('q'))
    clops.add(debugFilters.getAddOption("Add a class to debug, or specify a line with Class:lineno"), "debug")
    clops.add(boogie.getEnable("select Boogie backend"), "boogie")
    clops.add(chalice.getEnable("select Chalice backend"), "chalice")
    clops.add(silver.getAssign("select Silver backend (silicon/carbon)"), "silver")
    clops.add(silver.getAssign("select Silicon backend", "silicon"), "silicon")
    clops.add(silver.getAssign("select Carbon backend", "carbon"), "carbon")
    clops.add(dafny.getEnable("select Dafny backend"), "dafny")
    clops.add(check_defined.getEnable("check if defined processes satisfy their contracts."), "check-defined")
    clops.add(check_axioms.getEnable("check if defined processes satisfy their contracts."), "check-axioms")
    clops.add(check_history.getEnable("check if defined processes satisfy their contracts."), "check-history")
    clops.add(separate_checks.getEnable("validate classes separately"), "separate")
    clops.add(help_passes.getEnable("print help on available passes"), "help-passes")
    clops.add(sequential_spec.getEnable("sequential specification instead of concurrent"), "sequential")
    clops.add(pass_list_option, "passes")
    clops.add(show_before.getAppendOption("Show source code before given passes"), "show-before")
    clops.add(show_after.getAppendOption("Show source code after given passes"), "show-after")
    clops.add(show_file.getAssign("redirect show output to files instead of stdout"), "save-show")
    clops.add(debugBefore.getAddOption("Dump the COL AST before a pass is run"), "debug-before")
    clops.add(debugAfter.getAddOption("Dump the COL AST after a pass is run"), "debug-after")
    clops.add(stop_after.getAppendOption("Stop after given passes"), "stop-after")
    clops.add(strictInternalConditions.getEnable("Enable strict internal checks for AST conditions (expert option)"), "strict-internal")
    clops.add(explicit_encoding.getEnable("explicit encoding"), "explicit")
    clops.add_removed("the inline option was removed in favor of the inline modifer", "inline")
    clops.add(global_with_field.getEnable("Encode global access with a field rather than a parameter. (expert option)"), "global-with-field")
    clops.add(infer_modifies.getEnable("infer modifies clauses"), "infer-modifies")
    clops.add(no_context.getEnable("disable printing the context of errors"), "no-context")
    clops.add(gui_context.getEnable("enable the gui extension of the context"), "gui")
    clops.add(sat_check.getDisable("Disable checking if method pre-conditions are satisfiable"), "disable-sat")
    clops.add(abruptTerminationViaExceptions.getEnable("Force compilation of abrupt termination to exceptions"), "at-via-exceptions")
    clops.add(trigger_generation.getOptionalAssign("Try to simplify universal quantifiers and generate triggers for them."), "triggers")
    clops.add(learn.getEnable("Learn unit times for AST nodes."), "learn")
    CommandLineTesting.addOptions(clops)
    Configuration.add_options(clops)
    clops.parse(args)
  }

  private def setupLogging(): Unit = {
    import hre.lang.System.LogLevel

    var level = logLevel.get match {
      case "silent" => LogLevel.Silent
      case "abort" => LogLevel.Abort
      case "result" => LogLevel.Result
      case "warning" => LogLevel.Warning
      case "info" => LogLevel.Info
      case "progress" => LogLevel.Progress
      case "debug" => LogLevel.Debug
      case "all" => LogLevel.All
    }

    if (!debugFilters.get.isEmpty && level.getOrder < hre.lang.System.LogLevel.Debug.getOrder)
      level = hre.lang.System.LogLevel.Debug

    for (filter <- debugFilters.get.asScala.keys) {
      if (filter.contains(":") /* With line number */ ) hre.lang.System.addDebugFilterByLine(filter)
      else hre.lang.System.addDebugFilterByClassName(filter)
    }

    hre.lang.System.setOutputStream(System.out, level)
    hre.lang.System.setErrorStream(System.err, level)
    System.setErr(new ForbiddenPrintStream(System.err))
    System.setOut(new ForbiddenPrintStream(System.out))
  }

  private def checkOptions(): Unit = {
    if (version.get) {
      Output("%s %s", BuildInfo.name, BuildInfo.version)
      Output("Built by sbt %s, scala %s at %s", BuildInfo.sbtVersion, BuildInfo.scalaVersion, Instant.ofEpochMilli(BuildInfo.builtAtMillis))
      if (BuildInfo.currentBranch != "master")
        Output("On branch %s, commit %s, %s",
          BuildInfo.currentBranch, BuildInfo.currentShortCommit, BuildInfo.gitHasChanges)

      throw new HREExitException(0)
    }

    if (help_passes.get) {
      Output("The following passes are available:")
      Passes.BY_KEY.foreach {
        case (key, pass) => Output(" %-12s : %s", key, pass.description)
      }
      throw new HREExitException(0)
    }

    if(Seq(
      CommandLineTesting.enabled,
      boogie.get,
      chalice.get,
      silver.used,
      dafny.get,
      pass_list.asScala.nonEmpty
    ).forall(!_)) {
      Fail("no back-end or passes specified")
    }

    if (silver.used) silver.get match {
      case "silicon_qp" =>
        Warning("silicon_qp has been merged into silicon, using silicon instead")
        silver.set("silicon")
      case "silicon" =>
      case "carbon" =>
      case _ =>
        Fail("unknown silver backend: %s", silver.get)
    }
  }

  private def parseInputs(inputPaths: Array[String]): Unit = {
    Progress("parsing inputs...")
    report = new PassReport(new ProgramUnit)
    report.setOutput(report.getInput)
    report.add(new ErrorDisplayVisitor)

    tk.show
    for (name <- inputPaths) {
      val f = new File(name)
      if (!no_context.get) FileOrigin.add(name, gui_context.get)
      report.getOutput.add(Parsers.parseFile(f.getPath))
    }

    Progress("Parsed %d file(s) in: %dms", Int.box(inputPaths.length), Long.box(tk.show))

    if (boogie.get || sequential_spec.get)
      report.getOutput.setSpecificationFormat(SpecificationFormat.Sequential)
  }

  private def collectPassesForBoogie: Seq[AbstractPass] = {
    var passes = Seq(
      BY_KEY("java_resolve"), // inspect class path for retreiving signatures of called methods. Will add files necessary to understand the Java code.
      BY_KEY("standardize"), // a rewriter s.t. only a subset of col will have to be supported
      BY_KEY("check"), // type check col. Add annotations (the types) to the ast.
      BY_KEY("rewrite_arrays"), // array generation and various array-related rewrites
      BY_KEY("check"),
      BY_KEY("flatten"), // expressions that contain method calls (possibly having side-effects) are put into separate statements.
      BY_KEY("assign"), // '(x = y ==> assign(x,y);). Has not been merged with standardize because flatten needs to be done first.
      BY_KEY("finalize_args"), // declare new variables to never have to change the arguments (which isn't allowed in silver)
      BY_KEY("reorder"), // silver requires that local variables are declared at the top of methods (and loop-bodies?) so they're all moved to the top
    )

    if (infer_modifies.get) {
      passes ++= Seq(
        BY_KEY("standardize"),
        BY_KEY("check"),
        BY_KEY("modifies"), // modifies is mandatory. This is how to automatically add it
      )
    }

    passes ++= Seq(
      BY_KEY("standardize"),
      BY_KEY("check"),
      BY_KEY("voidcalls"), // all methods in Boogie are void, so use an out parameter instead of 'return..'
      BY_KEY("standardize"),
      BY_KEY("check"),
      BY_KEY("flatten"),
      BY_KEY("reorder"),
      BY_KEY("standardize"),
      BY_KEY("check"),
      BY_KEY("strip_constructors"), // somewhere in the parser of Java, constructors are added implicitly. They need to be taken out again.
      BY_KEY("standardize"),
      BY_KEY("check"),
      BY_KEY("boogie"), // run backend
    )

    passes
  }

  private def collectPassesForDafny: Seq[AbstractPass] = Seq(
    BY_KEY("java_resolve"),
    BY_KEY("standardize"),
    BY_KEY("check"),
    BY_KEY("voidcalls"),
    BY_KEY("standardize"),
    BY_KEY("check"),
    BY_KEY("dafny"),
  )

  /** From a starting set of features: tries to compute a valid ordering of the passes */
  def computePassChainFromPassSet(featuresIn: Set[Feature], passSet: Set[AbstractPass]): Option[Seq[AbstractPass]] = {
    Debug("== Trying to compute pass chain ==")
    val passesToDo: ArrayBuffer[AbstractPass] = ArrayBuffer() ++ passSet.toSeq.sortBy(_.key)
    var features = featuresIn
    var passes = ArrayBuffer.empty[AbstractPass]

    while(passesToDo.nonEmpty) {
      /* The next pass must permit the current features, and no pass must introduce the features it removes */
      val nextPassResults = passesToDo.map {
        case pass if (features -- pass.permits).nonEmpty =>
          Left(s"cannot apply ${pass.key} since it does not permit these features: ${features--pass.permits}'")
        case pass if passesToDo.exists(_.introduces.intersect(pass.removes).nonEmpty) =>
          val conflictingPass = passesToDo.find(_.introduces.intersect(pass.removes).nonEmpty).get
          Left(s"cannot apply ${pass.key} since ${conflictingPass.key} introduces ${conflictingPass.introduces.intersect(pass.removes)}")
        case pass => Right(pass)
      }

      val nextPass = nextPassResults.collectFirst {
        case Right(pass) => pass
      } match {
        case Some(pass) => pass
        case None =>
          /* No pass satisfies the above conditions. In this case, we could also execute a pass that removes a feature
             that is later introduced again. This imposes that all passes that do not permit the feature occur before
             passes that introduce the feature. */
          val allowedOrderImposingPasses = passesToDo.filter(pass =>
            (features -- pass.permits).isEmpty &&
              passesToDo.filter(pass2 => (pass.removes -- pass2.permits).nonEmpty).forall(_.introduces.intersect(pass.removes).isEmpty)
          )

          /* If there is exactly one such pass, there can only be a solution if we do the pass right now. */
          if(allowedOrderImposingPasses.size == 1) {
            val pass = allowedOrderImposingPasses.head
            Debug("vvvv Will impose that all of:  %s", passesToDo.filter(pass2 => (pass.removes -- pass2.permits).nonEmpty).map(_.key))
            Debug("vvvv Must occur before all of: %s", passesToDo.filter(pass2 => (pass.removes -- pass2.permits).isEmpty).map(_.key))
            pass
          } else {
            /* Otherwise, there may or may not be a solution, but this is expensive to compute. */
            Debug("Leftover features: %s", features)
            if(allowedOrderImposingPasses.nonEmpty)
              Debug("Perhaps we could have run one of: %s", allowedOrderImposingPasses)
            nextPassResults.foreach {
              case Left(error) => Debug(error)
              case _ =>
            }
            return None
          }
      }

      passesToDo -= nextPass
      Debug("Planning to do %s", nextPass.key)
      passes += nextPass
      features = features -- nextPass.removes ++ nextPass.introduces
    }

    Some(passes)
  }

  /** Collect all passes that remove a feature */
  def findPassesToRemove(feature: Feature): Set[AbstractPass] =
    BY_KEY.values.filter(_.removes.contains(feature)).toSet

  /** Yield all sets of passes that are at least able to remove all features that are not permitted in a pass */
  def removalFixedPoint(featuresIn: Set[Feature], passes: Set[AbstractPass]): Seq[Set[AbstractPass]] = {
    val allFeatures = passes.foldLeft(featuresIn)(_ ++ _.introduces)
    val sharedPermit = passes.foldLeft(Feature.ALL)(_ intersect _.permits)
    val removes = passes.foldLeft(Set.empty[Feature])(_ ++ _.removes)
    val toRemove = (allFeatures -- sharedPermit) -- removes

    if(toRemove.isEmpty) {
      Seq(passes)
    } else {
      val extraPasses = toRemove.map(findPassesToRemove).map {
        case passes if passes.isEmpty =>
          return Seq()
        case passes if passes.size == 1 =>
          passes.head
        case more =>
          return more.toSeq.flatMap(pass => removalFixedPoint(featuresIn, passes + pass))
      }

      removalFixedPoint(featuresIn, passes ++ extraPasses)
    }
  }

  def computeGoal(featuresIn: Set[Feature], goal: AbstractPass): Option[Seq[AbstractPass]] = {
    val passChains = removalFixedPoint(featuresIn, Set(goal)).flatMap(computePassChainFromPassSet(featuresIn, _))

    if(passChains.isEmpty) {
      None
    } else {
      Some(passChains.minBy(_.size))
    }
  }

  def collectPassesForSilver: Seq[AbstractPass] = {
    val resolve = Passes.BY_KEY.apply("java_resolve")
    val standardize = Passes.BY_KEY.apply("standardize")
    val check = Passes.BY_KEY.apply("check")
    val localCheck = Passes.BY_KEY("local-variable-check")

    Seq(resolve, standardize, check, localCheck).foreach(
      pass => report = pass.apply_pass(report, Array()))

    var features = Feature.scan(report.getOutput) ++ Set(
      // These are "gated" features: they are (too) hard to detect normally.
      vct.col.features.NotFlattened,
      vct.col.features.BeforeSilverDomains,
      vct.col.features.NullAsOptionValue,
      vct.col.features.NotOptimized,
      vct.col.features.DeclarationsNotLifted,
    ) ++ Set(
      // These are normal features, but need to run always for some reason
      vct.col.features.ScatteredDeclarations, // this pass finds duplicate names.
      vct.col.features.ImplicitLabels, // Can be detected, lazy, sorry
    )

    if(sat_check.get()) features += vct.col.features.NeedsSatCheck
    if(check_axioms.get()) features += vct.col.features.NeedsAxiomCheck
    if(check_defined.get()) features += vct.col.features.NeedsDefinedCheck
    if(check_history.get()) features += vct.col.features.NeedsHistoryCheck

    // intersperse type checks
    computeGoal(features, BY_KEY("silver")).get.flatMap(Seq(_, BY_KEY("java-check"))).init
  }

  private def getPasses: Seq[AbstractPass] = {
    val features = new FeatureScanner
    report.getOutput.accept(features)

    if (pass_list_option.used) {
      pass_list.asScala.map(key => BY_KEY.get(key) match {
        case None => Fail("Unknown pass: %s", key); ???
        case Some(pass) => pass
      }).toSeq
    }
    else if (boogie.get) collectPassesForBoogie
    else if (dafny.get) collectPassesForDafny
    else if (silver.used || chalice.get) collectPassesForSilver
    else { Fail("no back-end or passes specified"); ???}
  }

  private def show(pass: AbstractPass): Unit = {
    val name = show_file.get
    if (name != null) {
      val file = String.format(name, pass.key)
      val out = new PrintWriter(new FileOutputStream(file))
      vct.col.ast.util.Configuration.getDiagSyntax.print(out, report.getOutput)
      out.close()
    }
    else {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      vct.col.ast.util.Configuration.getDiagSyntax.print(out, report.getOutput)
      out.close()
    }
  }

  @throws[FileNotFoundException]
  private def doPasses(passes: Seq[AbstractPass]): Unit = {
    var lastPass: AbstractPass = null
    var featuresIn: Set[Feature] = null

    for((pass, i) <- passes.zipWithIndex) {
      if (debugBefore.has(pass.key)) report.getOutput.dump()
      if (show_before.contains(pass.key)) show(pass)

      tk.show
      report = pass.apply_pass(report, Array())
      Progress("[%02d%%] %s took %d ms", Int.box(100 * (i+1) / passes.size), pass.key, Long.box(tk.show))

      if(strictInternalConditions.get()) {
        if (pass.key != "java-check") {
          // Collect the input features and store which pass they belong to
          featuresIn = Feature.scan(report.getInput)
          lastPass = pass
        } else {
          // We have the input features and can (with types) now scan output features
          val featuresOut = Feature.scan(report.getOutput)

          val notRemoved = featuresOut.intersect(pass.removes)
          val extraIntro = featuresOut -- featuresIn -- pass.introduces

          if (notRemoved.nonEmpty) {
            Warning("Pass %s did not remove %s", lastPass.key, notRemoved.map(_.toString).mkString(", "))
          }
          if (extraIntro.nonEmpty) {
            Warning("Pass %s introduced %s", lastPass.key, extraIntro.map(_.toString).mkString(", "))
          }
        }
      }

      if (debugAfter.has(pass.key)) report.getOutput.dump()
      if (show_after.contains(pass.key)) show(pass)
      if (stop_after.contains(pass.key)) Fail("exit after pass %s", pass)

      if(report.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return
      }
    }

    Verdict("The final verdict is Pass")
  }

  private def run(args: Array[String]): Int = {
    var exit = 0
    val wallStart = System.currentTimeMillis
    tk = new TimeKeeper
    try {
      hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info)
      hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info)
      val inputPaths = parseOptions(args)
      setupLogging()
      checkOptions()
      if (CommandLineTesting.enabled) CommandLineTesting.runTests()
      else {
        parseInputs(inputPaths)
        doPasses(getPasses)
      }
    } catch {
      case e: HREExitException =>
        exit = e.exit
        Verdict("The final verdict is Error")
      case e: Throwable =>
        DebugException(e)
        Warning("An unexpected error occured in VerCors! "
              + "Please report an issue at https://github.com/utwente-fmt/vercors/issues/new. "
              + "You can see the full exception by adding '--debug vct.main.Main' to the flags.")
        Verdict("The final verdict is Error")
    } finally Progress("entire run took %d ms", Long.box(System.currentTimeMillis - wallStart))
    exit
  }
}