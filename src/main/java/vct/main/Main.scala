package vct.main

import hre.ast.FileOrigin
import hre.config._
import hre.io.ForbiddenPrintStream
import hre.lang.HREExitException
import hre.lang.System._
import hre.tools.TimeKeeper
import hre.util.Notifier
import vct.col.ast.stmt.decl.{GPUOptFlags, ProgramUnit}
import vct.col.features.{Feature, RainbowVisitor}
import vct.col.veymont.Util
import vct.experiments.learn.SpecialCountVisitor
import vct.logging.PassReport
import vct.main.Main.backend_option
import vct.main.Passes.BY_KEY
import vct.silver.ErrorDisplayVisitor
import vct.test.CommandLineTesting

import java.io._
import java.nio.file.Paths
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util
import scala.jdk.CollectionConverters._

object Main {
  val backend_option = new LinearCollectSetting

  var counters = new util.HashMap[String, SpecialCountVisitor]

  def main(args: Array[String]): Unit =
    new Main().run(args)

  //def main(args: Array[String]): Unit = Preprocessor.main(args)
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
  private val notifySetting = new BooleanSetting(false)

  private val pass_list = new StringListSetting
  private val pass_list_option = pass_list.getAppendOption("add to the custom list of compilation passes")
  private val stopAfter = new StringListSetting
  private val stopBeforeBackend = new BooleanSetting(false)
  private val stopAfterTypecheck = new BooleanSetting(false)
  private val strictInternalConditions = new BooleanSetting(false)
  private var inputPaths = Array.empty[String]

  private val silver = new StringSetting("silver")

  private val check_defined = new BooleanSetting(false)
  private val check_axioms = new BooleanSetting(false)
  private val check_history = new BooleanSetting(false)
  private val separate_checks = new BooleanSetting(false)
  private val global_with_field = new BooleanSetting(false)
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
    clops.add(silver.getAssign("select Silver backend (silicon/carbon)"), "silver")
    clops.add(silver.getAssign("select Silicon backend", "silicon"), "silicon")
    clops.add(silver.getAssign("select Carbon backend", "carbon"), "carbon")
    clops.add(check_defined.getEnable("Check if the process-algebraic specification itself satisfies its contract."), "check-defined")
    clops.add(check_axioms.getEnable("Check if defined processes satisfy their contracts."), "check-axioms")
    clops.add(check_history.getEnable("Check if the program correctly implements the process-algebraic specification."), "check-history")
    clops.add(separate_checks.getEnable("validate classes separately"), "separate")
    clops.add(help_passes.getEnable("print help on available passes"), "help-passes")
    clops.add(pass_list_option, "passes")
    clops.add(show_before.getAppendOption("Show source code before given passes"), "show-before")
    clops.add(show_after.getAppendOption("Show source code after given passes"), "show-after")
    clops.add(show_file.getAssign("redirect show output to files instead of stdout"), "save-show")
    clops.add(debugBefore.getAddOption("Dump the COL AST before a pass is run"), "debug-before")
    clops.add(debugAfter.getAddOption("Dump the COL AST after a pass is run"), "debug-after")
    clops.add(notifySetting.getEnable("Send a system notification upon completion"), "notify")
    clops.add(stopAfter.getAppendOption("Stop after given passes"), "stop-after")
    clops.add(stopBeforeBackend.getEnable("Only do parsing, typechecking, and AST transformations. Do not do verification with the backend."), "stop-before-backend")
    clops.add(stopAfterTypecheck.getEnable("Only do parsing and typechecking. Do not apply AST transformations, nor verification with the backend."), "stop-after-typecheck")
    clops.add(strictInternalConditions.getEnable("Enable strict internal checks for AST conditions (expert option)"), "strict-internal")
    clops.add(global_with_field.getEnable("Encode global access with a field rather than a parameter. (expert option)"), "global-with-field")
    clops.add(no_context.getEnable("disable printing the context of errors"), "no-context")
    clops.add(gui_context.getEnable("enable the gui extension of the context"), "gui")
    clops.add(sat_check.getDisable("Disable checking if method pre-conditions are satisfiable"), "disable-sat")
    clops.add(abruptTerminationViaExceptions.getEnable("Force compilation of abrupt termination to exceptions"), "at-via-exceptions")
    clops.add(trigger_generation.getOptionalAssign("Try to simplify universal quantifiers and generate triggers for them."), "triggers")
    clops.add(learn.getEnable("Learn unit times for AST nodes."), "learn")
    clops.add(backend_option.getAddOption("Options to pass to the backend"), "backend-option")
    CommandLineTesting.addOptions(clops)
    Configuration.add_options(clops)
    val parseres = clops.parse(args)
    if (Configuration.veymont.is(Configuration.veymont_decompose))
      parseres :+ Configuration.getVeyMontFiles.getAbsolutePath()
    else parseres
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
      printVersions()
    }

    if (help_passes.get) {
      Output("The following passes are available:")
      Passes.BY_KEY.foreach {
        case (key, pass) => Output(" %-12s : %s", key, pass.description)
      }
      throw new HREExitException(0)
    }

    if (Seq(
      CommandLineTesting.enabled,
      silver.used,
      pass_list.asScala.nonEmpty,
      !Configuration.gpu_optimizations.isEmpty,
      Configuration.veymont.get() != null,
    ).forall(!_)) {
      Fail("no back-end or passes specified")
    }

    if (stopBeforeBackend.get() && stopAfterTypecheck.get()) {
      Fail("The --stop-before-backend and --stop-after-typecheck flags are mutually exclusive.")
    }

    if (silver.used) silver.get match {
      case "silicon" => // Nothing to check for
      case "carbon" => // Nothing to check for
      case _ =>
        Fail("unknown silver backend: %s", silver.get)
    }
    if (Configuration.veymont.get() != null) {
      val vFile = inputPaths.headOption
      if (vFile.isDefined) {
        val nonPVL = inputPaths.filter(p => !p.endsWith(".pvl"))
        if (nonPVL.nonEmpty && !nonPVL.forall(_.endsWith(Configuration.javaChannelFile)))
          Fail("VeyMont cannot use non-PVL files %s", nonPVL.mkString(", "))
        if (!(vFile.get.endsWith(".pvl") || vFile.get.endsWith(".java")))
          Fail("VeyMont can only output to file %s, it only accepts files ending with '.pvl' or '.java'", vFile)
      }
    }
  }

  private def printVersions(): Unit = {
    Output("%s %s", BuildInfo.name, BuildInfo.version)

    val timestamp = {
      val instance = Instant.ofEpochMilli(BuildInfo.builtAtMillis)
      val localDateTime = java.time.LocalDateTime
        .ofInstant(instance, ZoneId.systemDefault())
      localDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    }

    Output("Built at %s", timestamp)
    if (BuildInfo.currentBranch != "master")
      Output("On branch %s, commit %s, %s",
        BuildInfo.currentBranch, BuildInfo.currentShortCommit, BuildInfo.gitHasChanges)

    val z3VersionLine = {
      val z3 = Configuration.getZ3
      z3.addArg("--version")
      val mp = z3.startProcess()
      mp.recv().getArg(0)
    }

    val boogieVersionLine = {
      val boogie = Configuration.getBoogie
      boogie.addArg("/version")
      val mp = boogie.startProcess()
      mp.recv().getArg(0)
    }

    val viperVersions = if (Set(BuildInfo.silverCommit, BuildInfo.siliconCommit, BuildInfo.carbonCommit).size == 1) {
      Seq(("viper", BuildInfo.silverCommit))
    } else {
      Seq(("silver", BuildInfo.silverCommit),
        ("silicon", BuildInfo.siliconCommit),
        ("carbon", BuildInfo.carbonCommit))
    }
    val viperVersionsTxt = viperVersions.map {
      case (name, commitId) => s"- $name: ${commitId.getOrElse("unknown")}"
    }

    val allVersions = viperVersionsTxt ++ Seq(
      s"- z3: $z3VersionLine",
      s"- boogie: $boogieVersionLine"
    )

    val allVersionsTxt = ("Versions:" +: allVersions).mkString("\n")

    Output("%s", allVersionsTxt)

    throw new HREExitException(0)
  }

  private def parseInputs(inputPaths: Array[String]): Unit = {
    Progress("Parsing inputs...")
    report = new PassReport(new ProgramUnit)
    report.setOutput(report.getInput)
    report.add(new ErrorDisplayVisitor)

    tk.show
    for (pathName <- inputPaths) {
      val path = Paths.get(pathName)
      if (!no_context.get) FileOrigin.add(path, gui_context.get)
      report.getOutput.add(Parsers.parseFile(path))
    }

    Progress("Parsed %d file(s) in: %dms", Int.box(inputPaths.length), Long.box(tk.show))

  }

  private def collectPassesForGPUOpts: Seq[AbstractPass] = {
    var passes = Seq(Passes.BY_KEY("splitCompositeDeclarations"), Passes.BY_KEY("checkTypesJava"))
    if (Configuration.gpu_optimizations.contains(GPUOptFlags.matrixLin.toString)) {
      passes ++= Seq(Passes.BY_KEY("linearizeMatrices"))
      passes ++= Seq(Passes.BY_KEY("checkTypesJava"))
    }
    if (Configuration.gpu_optimizations.contains(GPUOptFlags.fusion.toString)) {
      passes ++= Seq(Passes.BY_KEY("fuseKernels"))
      passes ++= Seq(Passes.BY_KEY("checkTypesJava"))
    }
    if (Configuration.gpu_optimizations.contains(GPUOptFlags.iterMerge.toString)) {
      passes ++= Seq(Passes.BY_KEY("mergeLoopIterations"))
      passes ++= Seq(Passes.BY_KEY("checkTypesJava"))
    }
    if (Configuration.gpu_optimizations.contains(GPUOptFlags.dataLoc.toString)) {
      passes ++= Seq(Passes.BY_KEY("globalMemoryLocToRegister"))
      passes ++= Seq(Passes.BY_KEY("checkTypesJava"))
    }
    if (Configuration.gpu_optimizations.contains(GPUOptFlags.tiling.toString)) {
      passes ++= Seq(Passes.BY_KEY("tileKernel"))
      passes ++= Seq(Passes.BY_KEY("checkTypesJava"))
    }
    if (Configuration.gpu_optimizations.contains(GPUOptFlags.loopUnrolling.toString)) {
      passes ++= Seq(Passes.BY_KEY("unrollLoops"))
      passes ++= Seq(Passes.BY_KEY("checkTypesJava"))
      passes ++= collectPassesForSilver
    }
    passes ++= Seq(Passes.BY_KEY("printGpuOptOut"))
    passes
  }

  private def collectPassesForVeyMontPre : Seq[AbstractPass] = {
    silver.set("silicon")
    Seq(
      BY_KEY("VeyMontStructCheck"),
      BY_KEY("VeyMontTerminationCheck"),
      BY_KEY("VeyMontGlobalProgPerms"),
      BY_KEY("VeyMontPrintAnnotatedProg"))
  }

  private def collectPassesForVeyMontPost : Seq[AbstractPass] = {
    silver.set("silicon")
    Seq(
      BY_KEY("VeyMontDecompose"),
      BY_KEY("removeEmptyBlocks"),
      BY_KEY("VeyMontLocalProgConstr"),
      BY_KEY("VeyMontAddStartThreads"),
      BY_KEY("VeyMontPrintOutput"))
  }

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
    "removeIgnoredElements",
    "splitCompositeDeclarations",
    "resolveTypeExpressions",
    "loadExternalClasses",
    "stringClassToPrimitive",
    "standardize",
    "interpretMethodAnnotations",
    "wrapTopLevelDeclarations",
    "removeUnusedExternMethods",
    "encodeLockInvariantProof",
    "synchronizedToTryFinally",
    "encodeForkLockWait",
    "specifyImplicitLoopLabels",
    "switchToIfChain",
    "addDefaultConstructor",
    "propagateAbstractMethodContracts",
    "arrayNullValuesToNone",
    "finalizeArguments",
    "actionHeaderToActionBlock",
    "addRequirementSatCheck",
    "pureMethodsToFunctions",
    "sortWithThen",
    Choose(
      Seq(),
      Seq("dereferenceToFieldAccess", "checkHistory"),
      Seq("dereferenceToFieldAccess", "checkAxioms"),
      Seq("dereferenceToFieldAccess", "checkDefined"),
    ),
    "desugarADTOperators",
    "inlineAssignmentToStatement",
    "continueToBreak",
    Choose(
      Seq("breakReturnToExceptions"),
      Seq("breakReturnToGoto"),
    ),
    "encodeCurrentThread",
    "collectStaticFields",
    "inferADTElementTypes",
    "encodeKernelClass",
    "checkAssignInPar",
    "encodeMagicWands",
    "inline",
    "inlineAtomicMethods",
    "openMPToParallelBlocks",
    "propagateInvariants",
    "dummy-InvariantsPropagatedHere",
    "compileToJava",
    "liftGhostCode",
    "inlineWithThenHints",
    "inlineParallelAtomics",
    "encodeParallelBlocks",
    Choose(
      Seq("stackLocationsToHeapLocations", "pointersToArraysLifted"),
      Seq("pointersToArrays"),
    ),
    "desugarValidPointer",
    "simplify",
    "simplifySums",
    "optimizeForSilver",
    "encodeVectorBlocks",
    "adtOperatorsToFunctions",
    "introExcVar",
    "desugarArrayOps",
    "flattenNestedExpressions",
    "encodeInheritanceToDomain",
    "tryThrowSignalsToGoto",
    "importADTsAndRefEncode",
    "returnTypeToOutParameter",
    "reduceQuantifierNesting",
    "inlinePatternsToTriggers",
    "generateQuantifierTriggers",
    "scaleAllPredicateApplications",
    "collectInnerDeclarations",
    "collectDeclarations",
  )

  def validChain(chain: Seq[AbstractPass], featuresIn: Set[Feature]): Boolean = {
    var features = featuresIn

    for(pass <- chain) {
      if((features -- pass.permits).nonEmpty) {
        Debug(s"Rejecting because ${pass.key} does not allow ${features -- pass.permits}")
        return false
      }

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

  def computeGoal(featuresIn: Set[Feature]): scala.Option[Seq[AbstractPass]] = {
    val toolPass = silver.get() match {
      case "carbon" => BY_KEY("applyCarbon")
      case "silicon" => BY_KEY("applySilicon")
    }

    // Expand all choices
    val chains = ChainPart.inflate(silverPassOrder).map(_.map(BY_KEY(_)) :+ toolPass)

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
    report = Passes.BY_KEY("checkTypesJava").apply_pass(report, Array())

    var features = Feature.scan(report.getOutput) ++ Set(
      // These are "gated" features: they are (too) hard to detect normally.
      vct.col.features.NotFlattened,
      vct.col.features.BeforeSilverDomains,
      vct.col.features.NullAsOptionValue,
      vct.col.features.NotOptimized,
      vct.col.features.DeclarationsNotLifted,
      vct.col.features.ParallelLocalAssignmentNotChecked,
      vct.col.features.NotJavaResolved,
      vct.col.features.InvariantsPropagatedHere,
    ) ++ Set(
      // These are normal features, but need to run always for some reason
      vct.col.features.ScatteredDeclarations, // this pass finds duplicate names (even if they're not scattered)
      vct.col.features.ImplicitLabels, // Can be detected, lazy, sorry
    )

    if(features.contains(vct.col.features.Extern))
      /* too hard to detect whether an extern decl is used, but it needs top-level-decls and we don't want to run that
       * for the silver frontend. */
      features += vct.col.features.UnusedExtern

    // options are encoded as gated features
    if(sat_check.get()) features += vct.col.features.NeedsSatCheck
    if(check_axioms.get()) features += vct.col.features.NeedsAxiomCheck
    if(check_defined.get()) features += vct.col.features.NeedsDefinedCheck
    if(check_history.get()) features += vct.col.features.NeedsHistoryCheck

    val passes = if (stopAfterTypecheck.get()) {
      Seq()
    } else {
      computeGoal(features).get
      computeGoal(features).get
    }

    if (stopBeforeBackend.get()) {
      // We drop the last pass, which happens to be the silicon/carbon pass
      passes.init
    } else {
      passes
    }
  }

  private def getPasses: Seq[AbstractPass] = {
    if (pass_list_option.used) {
      pass_list.asScala.map(key => BY_KEY.get(key) match {
        case None => Fail("Unknown pass: %s", key); ???
        case Some(pass) => pass
      }).toSeq
    }
    else if (!Configuration.gpu_optimizations.isEmpty) collectPassesForGPUOpts
    else if (silver.used) collectPassesForSilver
    else if (Configuration.veymont.get() != null && Configuration.veymont.is(Configuration.veymont_check)) collectPassesForSilver
    else if (Configuration.veymont.get() != null && Configuration.veymont.is(Configuration.veymont_decompose)) collectPassesForVeyMontPost
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

  private def doPasses(passes: Seq[AbstractPass]): Int = {
    report = BY_KEY("checkTypesJava").apply_pass(report, Array())
    for((pass, i) <- passes.zipWithIndex) {
      if (debugBefore.has(pass.key)) report.getOutput.dump()
      if (show_before.contains(pass.key)) show(pass)

      val featuresIn = if(strictInternalConditions.get()) {
        Feature.scan(report.getInput)
      } else { Set.empty }

      tk.show
      report = pass.apply_pass(report, if(Configuration.veymont.get() != null) inputPaths else Array())

      if(report.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return report.getFatal
      }

      Progress("[%02d%%] %s took %d ms", Int.box(100 * (i+1) / passes.size), pass.key, Long.box(tk.show))

      if (debugAfter.has(pass.key)) report.getOutput.dump()
      if (show_after.contains(pass.key)) show(pass)
      if (stopAfter.contains(pass.key)) Fail("exit after pass %s", pass)

      report = BY_KEY("checkTypesJava").apply_pass(report, Array())

      if(report.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return report.getFatal
      }

      if(strictInternalConditions.get()) {
        val scanner = new RainbowVisitor(report.getOutput)
        scanner.source().accept(scanner)
        val featuresOut = scanner.features.toSet

        val notRemoved = featuresOut.intersect(pass.removes)
        val extraIntro = (featuresOut -- featuresIn) -- pass.introduces

        val permissiveFeatures = Set(
          vct.col.features.QuantifierWithoutTriggers,
          vct.col.features.NestedQuantifiers,
          vct.col.features.ExceptionalReturn,
          vct.col.features.ContextEverywhere,
        )

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

        if((notRemoved -- permissiveFeatures).nonEmpty || (extraIntro -- permissiveFeatures).nonEmpty) {
          Abort("Halting, because strict internal conditions are enabled.")
        }
      }

    }

    Verdict("The final verdict is Pass")
    0
  }

  def run(args: Array[String]): Int = {
    var exit = 0
    val wallStart = System.currentTimeMillis
    tk = new TimeKeeper
    try {
      hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info)
      hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info)
      inputPaths = parseOptions(args)
      setupLogging()
      checkOptions()
      if (CommandLineTesting.enabled) CommandLineTesting.runTests()
      else {
        var newArgs = Array[String]()
        if(Configuration.veymont.get() != null && Configuration.veymont.is(Configuration.veymont_check)) {
          newArgs = runVeyMontPrePasses(args)
        }
        val verCorsStart = System.currentTimeMillis
        parseInputs(inputPaths)
        exit = doPasses(getPasses)
        if(Configuration.veymont.get() != null && Configuration.veymont.is(Configuration.veymont_check)) {
          Progress("[VeyMont] VerCors took %d ms",Long.box(System.currentTimeMillis - verCorsStart) )
          runVeyMontPostPasses(newArgs)
        }
      }
    } catch {
      case e: HREExitException =>
        exit = e.exit
        if(exit != 0)
          Verdict("The final verdict is Error")
      case e: Throwable =>
        exit = -180614
        DebugException(e)
        Warning("An unexpected error occured in VerCors! "
              + "Please report an issue at https://github.com/utwente-fmt/vercors/issues/new. "
              + "You can see the full exception by adding '--debug vct.main.Main' to the flags.")
        Verdict("The final verdict is Error")
    } finally {
      Progress("entire run took %d ms", Long.box(System.currentTimeMillis - wallStart))
      if(notifySetting.get()) {
        Notifier.notify("VerCors", "Verification is complete")
      }
    }
    exit
  }

  private def runVeyMontPrePasses(args: Array[String]) : Array[String] = {
    parseInputs(inputPaths)
    doPasses(collectPassesForVeyMontPre)
    val veymontIndex = Configuration.getVeyMontArgIndex(args);
    args.update(veymontIndex + 1, Util.getAnnotatedFileName(args(veymontIndex + 1)))
    var removeIndex = -1; //remove other files than new -glob file in arguments
    for(i <- veymontIndex+2 until args.length) {
      if(args(i).endsWith(".pvl"))
        removeIndex = i
    }
    val newArgs = if(removeIndex == -1) args else args.slice(0,veymontIndex+2) ++ args.slice(removeIndex+1,args.length)
    inputPaths = parseOptions(newArgs)
    checkOptions()
    newArgs
  }

  private def runVeyMontPostPasses(args: Array[String]) : Unit = {
    val veymontIndex = Configuration.getVeyMontArgIndex(args);
    args.update(veymontIndex, "--" + Configuration.veymont_decompose)
    inputPaths = parseOptions(args)
    checkOptions()
    parseInputs(inputPaths)
    doPasses(collectPassesForVeyMontPost)
  }
}