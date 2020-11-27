// -*- tab-width:2 ; indent-tabs-mode:nil -*-
// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.main

import java.io._
import java.time.Instant
import java.util
import hre.ast.FileOrigin
import hre.config.{BooleanSetting, ChoiceSetting, CollectSetting, Configuration, IntegerSetting, OptionParser, StringListSetting, StringSetting}
import hre.lang.HREExitException
import hre.lang.System._
import hre.tools.TimeKeeper
import vct.col.ast.stmt.decl.{ASTClass, Method, ProgramUnit, SpecificationFormat}
import vct.col.util.FeatureScanner
import vct.experiments.learn.SpecialCountVisitor
import vct.logging.PassReport
import vct.silver.ErrorDisplayVisitor
import hre.io.ForbiddenPrintStream
import vct.col.features.{Feature, RainbowVisitor}
import vct.main.Passes.BY_KEY
import vct.test.CommandLineTesting

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object Main {
  var counters = new util.HashMap[String, SpecialCountVisitor]

  def main(args: Array[String]): Unit = System.exit(new Main().run(args))
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

  object ChainPart {
    def inflate(parts: Seq[ChainPart]): Seq[Seq[String]] =
      parts.headOption match {
        case None => Seq(Seq())
        case Some(pass: Do) => inflate(parts.tail).map(pass.key +: _)
        case Some(Choose(alts@_* /* collect varargs into alts */)) =>
          val tail = inflate(parts.tail)
          alts.map(inflate).flatMap(branch => branch.flatMap(choice => tail.map(choice ++ _)))
      }
  }
  sealed trait ChainPart
  implicit sealed class Do(val key: String) extends ChainPart
  case class Choose(choices: Seq[ChainPart]*) extends ChainPart

  val silverPassOrder: Seq[ChainPart] = Seq(
    "spec-ignore",
    "flatten_variable_declarations",
    "string-class",
    "type-expressions",
    "java_resolve",
    "standardize",
    "interpret-annotations",
    "unused-extern",
    "top-level-decls",
    "lock-invariant-proof",
    "unfold-synchronized",
    "pvl-encode",
    "specify-implicit-labels",
    "unfold-switch",
    "zero-constructor",
    "java-encode",
    "array_null_values",
    "finalize_args",
    "action-header",
    "sat_check",
    "standardize-functions",
    Choose(
      Seq(),
      Seq("access", "check-history"),
      Seq("access", "check-axioms"),
      Seq("access", "check-defined"),
    ),
    "adt_operator_rewrite",
    "assign",
    "chalice-optimize",
    "chalice-preprocess",
    "continue-to-break",
    Choose(
      Seq("break-return-to-exceptions"),
      Seq("break-return-to-goto"),
    ),
    "current_thread",
    "globalize",
    "infer_adt_types",
    "kernel-split",
    "local-variable-check",
    "magicwand",
    "inline",
    "openmp2pvl",
    "propagate-invariants",
    "pvl-compile",
    "simplify_quant_relations",
    "sort-before-after",
    "csl-encode",
    "ghost-lift",
    "flatten_before_after",
    "inline-atomic",
    "parallel_blocks",
    "lift_declarations",
    "pointers_to_arrays_lifted",
    "desugar_valid_pointer",
    "simplify_quant",
    "simplify_sums",
    "silver-optimize",
    "vector-encode",
    "add-type-adt",
    "generate_adt_functions",
    "intro-exc-var",
    "rewrite_arrays",
    "flatten",
    "encode-try-throw-signals",
    "inline-pattern-to-trigger",
    "silver-class-reduction",
    "create-return-parameter",
    "quant-optimize",
    "gen-triggers",
    "scale-always",
    "silver-reorder",
    "reorder",
    "silver",
  )

  def validChain(chain: Seq[AbstractPass], featuresIn: Set[Feature]): Boolean = {
    var features = featuresIn

    for(pass <- chain) {
      if((features -- pass.permits).nonEmpty)
        return false

      features ++= pass.introduces
      features --= pass.removes
    }

    true
  }

  def minimize(chain: Seq[AbstractPass], featuresIn: Set[Feature]): Seq[AbstractPass] = {
    for(toRemove <- 0 until chain.size-1) {
      val newChain = chain.take(toRemove) ++ chain.drop(toRemove + 1)
      if(validChain(newChain, featuresIn)) {
        return minimize(newChain, featuresIn)
      }
    }
    chain
  }

  def filterNopPasses(chain: Seq[AbstractPass], featuresIn: Set[Feature]): Seq[AbstractPass] = {
    var features = featuresIn

    for(toRemove <- 0 until chain.size-1) {
      if(chain(toRemove).removes.intersect(features).isEmpty) {
        val newChain = chain.take(toRemove) ++ chain.drop(toRemove + 1)
        return filterNopPasses(newChain, featuresIn)
      }

      features ++= chain(toRemove).introduces
      features --= chain(toRemove).removes
    }

    chain
  }

  def computeGoal(featuresIn: Set[Feature]): Option[Seq[AbstractPass]] = {
    // Expand all choices
    val chains = ChainPart.inflate(silverPassOrder).map(_.map(BY_KEY(_)))

    // Filter out passes that don't remove anything (even before the chain is valid)
    val filteredChains = chains.map(filterNopPasses(_, featuresIn))

    // Filter for valid chains
    val validChains = filteredChains.filter(validChain(_, featuresIn))

    // Remove any passes that when removed constitute a valid chain
    val minimalChains = validChains.map(minimize(_, featuresIn))
    if(minimalChains.nonEmpty) {
      Some(minimalChains.minBy(_.size))
    } else {
      None
    }
  }

  def collectPassesForSilver: Seq[AbstractPass] = {
    if (Configuration.session_file.get() != null) {
      report = Passes.BY_KEY("pvl").apply_pass(report, Array())
    }

    if (Configuration.enable_gpu_optimizations.get()) {
      report = Passes.BY_KEY("unroll_loops").apply_pass(report, Array())
      show(Passes.BY_KEY("unroll_loops"))
//      report = Passes.BY_KEY("pvl").apply_pass(report, Array())
      return Seq.empty;
    }


    report = Passes.BY_KEY("java-check").apply_pass(report, Array())

    var features = Feature.scan(report.getOutput) ++ Set(
      // These are "gated" features: they are (too) hard to detect normally.
      vct.col.features.NotFlattened,
      vct.col.features.BeforeSilverDomains,
      vct.col.features.NullAsOptionValue,
      vct.col.features.NotOptimized,
      vct.col.features.DeclarationsNotLifted,
      vct.col.features.UnusedExtern,
      vct.col.features.ParallelLocalAssignmentNotChecked,
      vct.col.features.NotJavaResolved,
    ) ++ Set(
      // These are normal features, but need to run always for some reason
      vct.col.features.ScatteredDeclarations, // this pass finds duplicate names (even if they're not scattered)
      vct.col.features.ImplicitLabels, // Can be detected, lazy, sorry
    )

    // options are encoded as gated features
    if(sat_check.get()) features += vct.col.features.NeedsSatCheck
    if(check_axioms.get()) features += vct.col.features.NeedsAxiomCheck
    if(check_defined.get()) features += vct.col.features.NeedsDefinedCheck
    if(check_history.get()) features += vct.col.features.NeedsHistoryCheck

    computeGoal(features).get
  }

  private def getPasses: Seq[AbstractPass] = {
    if (pass_list_option.used) {
      pass_list.asScala.map(key => BY_KEY.get(key) match {
        case None => Fail("Unknown pass: %s", key); ???
        case Some(pass) => pass
      }).toSeq
    }
    else if (boogie.get) collectPassesForBoogie
    else if (dafny.get) collectPassesForDafny
    else if (silver.used || chalice.get) collectPassesForSilver
    else { Fail("no back-end or passes specified"); ??? }
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

  private def doPasses(passes: Seq[AbstractPass]): Unit = {
    Output("%s", passes)

    for((pass, i) <- passes.zipWithIndex) {
      if (debugBefore.has(pass.key)) report.getOutput.dump()
      if (show_before.contains(pass.key)) show(pass)

      val featuresIn = if(strictInternalConditions.get()) {
        Feature.scan(report.getInput)
      } else { Set.empty }

      tk.show
      report = pass.apply_pass(report, Array())

      if(report.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return
      }

      Progress("[%02d%%] %s took %d ms", Int.box(100 * (i+1) / passes.size), pass.key, Long.box(tk.show))

      report = BY_KEY("java-check").apply_pass(report, Array())

      if(report.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return
      }

      if(strictInternalConditions.get()) {
        val scanner = new RainbowVisitor(report.getOutput)
        scanner.source().accept(scanner)
        val featuresOut = scanner.features

        val notRemoved = featuresOut.intersect(pass.removes) -- Set(vct.col.features.QuantifierWithoutTriggers)
        val extraIntro = (featuresOut -- featuresIn) -- pass.introduces

        if (notRemoved.nonEmpty) {
          notRemoved.foreach(feature => {
            Output("Pass %s did not remove %s:", pass, feature)
            scanner.logBlameExamples(feature)
          })
        }

        if (extraIntro.nonEmpty) {
          extraIntro.foreach(feature => {
            Output("Pass %s introduced %s", pass, feature)
            scanner.logBlameExamples(feature)
          })
        }

        if(notRemoved.nonEmpty || extraIntro.nonEmpty) {
          Abort("Halting, because strict internal conditions are enabled.")
        }
      }

      if (debugAfter.has(pass.key)) report.getOutput.dump()
      if (show_after.contains(pass.key)) show(pass)
      if (stop_after.contains(pass.key)) Fail("exit after pass %s", pass)
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