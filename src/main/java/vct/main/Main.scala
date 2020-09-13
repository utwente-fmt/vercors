// -*- tab-width:2 ; indent-tabs-mode:nil -*-
// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.main

import java.io._
import java.time.Instant
import java.util
import java.util.concurrent.LinkedBlockingDeque

import hre.ast.FileOrigin
import hre.config.{Configuration, _}
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
      if (!(BuildInfo.currentBranch == "master"))
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

    if (!(CommandLineTesting.enabled || boogie.get || chalice.get || silver.used || dafny.get || pass_list.iterator.hasNext))
      Fail("no back-end or passes specified")

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

  private def collectPassesForBoogie(passes: util.Deque[String]): Unit = {
    passes.add("java_resolve") // inspect class path for retreiving signatures of called methods. Will add files necessary to understand the Java code.
    passes.add("standardize") // a rewriter s.t. only a subset of col will have to be supported
    passes.add("check") // type check col. Add annotations (the types) to the ast.
    passes.add("rewrite_arrays") // array generation and various array-related rewrites
    passes.add("check")
    passes.add("flatten") // expressions that contain method calls (possibly having side-effects) are put into separate statements.
    passes.add("assign") // '(x = y ==> assign(x,y);). Has not been merged with standardize because flatten needs to be done first.
    passes.add("finalize_args") // declare new variables to never have to change the arguments (which isn't allowed in silver)
    passes.add("reorder") // silver requires that local variables are declared at the top of methods (and loop-bodies?) so they're all moved to the top
    if (infer_modifies.get) {
      passes.add("standardize")
      passes.add("check")
      passes.add("modifies") // modifies is mandatory. This is how to automatically add it
    }
    passes.add("standardize")
    passes.add("check")
    passes.add("voidcalls") // all methods in Boogie are void, so use an out parameter instead of 'return..'
    passes.add("standardize")
    passes.add("check")
    passes.add("flatten")
    passes.add("reorder")
    passes.add("standardize")
    passes.add("check")
    passes.add("strip_constructors") // somewhere in the parser of Java, constructors are added implicitly. They need to be taken out again.
    passes.add("standardize")
    passes.add("check")
    passes.add("boogie") // run backend
  }

  private def collectPassesForDafny(passes: util.Deque[String]): Unit = {
    passes.add("java_resolve")
    passes.add("standardize")
    passes.add("check")
    passes.add("voidcalls")
    passes.add("standardize")
    passes.add("check")
    passes.add("dafny")
  }

  private def collectPassesForSilver(passes: util.Deque[String], features: FeatureScanner): Unit = {
    passes.add("java_resolve")
    if (silver.used && (features.usesSpecial(ASTSpecial.Kind.Lock) || features.usesSpecial(ASTSpecial.Kind.Unlock) || features.usesSpecial(ASTSpecial.Kind.Fork) || features.usesSpecial(ASTSpecial.Kind.Join) || features.usesOperator(StandardOperator.PVLidleToken) || features.usesOperator(StandardOperator.PVLjoinToken))) passes.add("pvl-encode") // translate built-in statements into methods and fake method calls.
    passes.add("standardize")
    passes.add("java-check") // marking function: stub
    passes.add("rainbow")
    //        if (features.usesOperator(StandardOperator.AddrOf)) {
    //            passes.add("lift_declarations");
    //        }
    //
    //        passes.add("check");
    //        passes.add("infer_adt_types");
    //        passes.add("adt_operator_rewrite");
    //        passes.add("standardize");
    //        passes.add("java-check");
    //        passes.add("pointers_to_arrays");
    //        passes.add("desugar_valid_pointer");
    //        passes.add("array_null_values"); // rewrite null values for array types into None
    //        if (silver.used()) {
    //            // The new encoding does not apply to Chalice yet.
    //            // Maybe it never will.
    //            passes.add("java-encode"); // disambiguate overloaded stuff, copy inherited functions and specifications
    //            passes.add("standardize");
    //            passes.add("check");
    //        if (sat_check.get()) {
    //            passes.add("sat_check"); // sanity check to avoid uncallable methods (where False is required)
    //        if (features.usesIterationContracts() || features.usesPragma("omp")) {
    //            passes.add("openmp2pvl"); // Converts *all* parallel loops! (And their compositions) Into ordered set of parallel blocks in pvl.
    //        passes.add("propagate-invariants"); // desugar \invariant (see \invariant documentation)
    //        if (features.usesOperator(StandardOperator.Wand)) {
    //            passes.add("magicwand"); // translate magicwand uses (proof oblications) into method calls
    //        passes.add("inline"); // inline predicates that are marked as inline (so 'fold/unfold' needn't be done)
    //        if (features.usesCSL()) {
    //            passes.add("csl-encode"); //
    //        if (features.hasVectorBlocks() || features.usesPragma("omp")) {
    //            passes.add("vector-encode"); // vector operations for supporting Saeed's paper
    //            if (features.usesIterationContracts() || features.usesParallelBlocks() || features.usesCSL() || features.usesPragma("omp")) {
    //                passes.add("parallel_blocks"); // pvl parallel blocks are put in separate methods that can be verified seperately. Method call replaces the contract of this parallel block.
    //                passes.add("standardize");
    //            }
    //            passes.add("simplify_quant"); // reduce nesting of quantifiers
    //            passes.add("simplify_quant_relations");
    //            if (features.usesSummation() || features.usesIterationContracts()) {
    //                passes.add("check");
    //                passes.add("simplify_sums"); // set of rewrite rules for removing summations
    //        if (features.usesKernels()) {// 8 feb 2018: is this now dead code (to be)? (SB)
    //            passes.add("kernel-split");
    //            passes.add("simplify_quant");
    //        boolean has_type_adt = false;
    //            if (features.usesOperator(StandardOperator.Instance)
    //                    || features.usesInheritance()
    //                    || features.usesOperator(StandardOperator.TypeOf)
    //            ) {
    //                passes.add("add-type-adt"); // add java's types of the programs as silicon's axiomatic datatypes
    //                has_type_adt = true;
    //        if (!silver.used() && features.usesInheritance()) { // 8 feb 2018: SB nominates this block for removal
    //            // reason: chalice's types and inheritance mechanism isn't the same as Java's, so why not translate everything the same way and ignore chalice's mechanism
    //            passes.add("ds_inherit"); // Use the old inheritance encoding for Chalice.
    //        // histories and futures
    //        // three verification runs:
    //        // 3) verify program: promises wrt process algebra need to be met
    //        // 2) verify the process algebra axioms: check whether paralel compositions are 'correct'
    //        // 1) auxiliarry definitions in the process algebra should match their contracts.
    //        if (check_defined.get()) {
    //            passes.add("check-defined"); // add checks
    //        } else if (check_axioms.get()) {
    //            passes.add("check-axioms");
    //        } else if (features.usesProcesses() || check_history.get()) {
    //            passes.add("access"); // pre-process for check-history?
    //            passes.add("check-history");
    //        if (explicit_encoding.get()) {
    //            passes.add("explicit_encoding"); // magic wand paper: for passing around predicates witnesses. In chalice predicates do not have arguments, except 'this'. So we're making data-types to pass around witnesses. Not necessary for silicon.
    //            passes.add("current_thread"); // add argument 'current thread' to all methods
    //        passes.add("rewrite_arrays"); // array generation and various array-related rewrites
    //        passes.add("generate_adt_functions");
    //        passes.add("flatten");
    //        passes.add("assign");
    //        passes.add("reorder");
    //        passes.add("simplify_quant");
    //            if (trigger_generation.get() > 0) {
    //                passes.add("simple_triggers=" + trigger_generation.get());
    //            passes.add("silver-class-reduction"); // remove the class (since all names are now unique), only one class remains
    //        } else {
    //            passes.add("globalize"); // create a separate class to contain all statics (class probably called 'Global', needs to be given as argument to the other methods)
    //            passes.add("rm_cons"); // replace constructors by init-methods
    //        if (has_type_adt) {
    //            passes.add("voidcallsthrown"); // like voidcalls, but also exceptions are put into an out-argument
    //            passes.add("voidcalls");
    //            passes.add("ghost-lift"); // change ghost code into real code so it can is used in the check
    //        passes.add("reorder"); // leaves declarations at the top of blocks (within loops and branches)
    //        passes.add("flatten_before_after"); // method calls can have 'before/after' blocks of ghost-code attached. Put it around all method calls.
    //            passes.add("silver-reorder"); // no declarations in branches (only in loops)
    //        if (!silver.used()) {
    //            passes.add("finalize_args");
    //            passes.add("reorder");
    //            passes.add("scale-always"); // syntax: in silicon [p]predicate() is mandatory, so change pred() to [1]pred()
    //            passes.add("check"); // the rewrite system needs a type check
    //            passes.add("silver-optimize"); // rewrite to things that silver likes better
    //            passes.add("quant-optimize"); // some illegal-quantifier constructions need to be written differently (plus optimize)
    //            passes.add("standardize-functions"); // pure methods do not need to be 'methods', try turning them into functions so silver and chalice can reason more intelligently about them. Pure methods can be used in specifications through this.
    //            passes.add("silver=" + silver.get());
    //        } else { //CHALICE
    //            passes.add("check"); // rewrite system needs a type check
    //            passes.add("chalice-optimize");
    //            passes.add("standardize-functions");
    //            passes.add("chalice-preprocess");
    //            passes.add("chalice");
    //        if (learn.get()) {
    //            passes.addFirst("count=" + silver.get() + "_before_rewrite");
    //            passes.add("learn=" + wallStart);
  }

  private def getPasses: LinkedBlockingDeque[String] = {
    val features = new FeatureScanner
    report.getOutput.accept(features)
    val passes = new LinkedBlockingDeque[String]
    if (pass_list_option.used) {
      for (s <- pass_list.asScala) {
        passes.add(s)
      }
    }
    else if (boogie.get) collectPassesForBoogie(passes)
    else if (dafny.get) collectPassesForDafny(passes)
    else if (silver.used || chalice.get) collectPassesForSilver(passes, features)
    else Abort("no back-end or passes specified")
    passes
  }

  private def show(pass: String): Unit = {
    val name = show_file.get
    if (name != null) {
      val file = String.format(name, pass)
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
  private def doPasses(passes: util.Deque[String]): Unit = {
    var fatal_errs = 0
    val passCount = passes.size
    while (!passes.isEmpty && fatal_errs == 0) {
      var pass = passes.removeFirst()
      var pass_args = pass.split("=")
      pass = pass_args(0)
      if (pass_args.length == 1) pass_args = new Array[String](0)
      else pass_args = pass_args(1).split("\\+")
      if (debugBefore.has(pass)) report.getOutput.dump()
      if (show_before.contains(pass)) show(pass)
      Passes.BY_KEY.get(pass) match {
        case None => Fail("unknown pass %s", pass)
        case Some(task) =>
          tk.show
          report = task.apply_pass(report, pass_args)
          fatal_errs = report.getFatal
          Progress("[%02d%%] %s took %d ms", Int.box(100 * (passCount - passes.size) / passCount), pass, Long.box(tk.show))
      }
      if (debugAfter.has(pass)) report.getOutput.dump()
      if (show_after.contains(pass)) show(pass)
      if (stop_after.contains(pass)) Fail("exit after pass %s", pass)
    }
    Verdict("The final verdict is %s", if (fatal_errs == 0) "Pass"
    else "Fail")
  }

  // TODO (Bob): None.get exception here is possible if you forget things like me
  def findPassToRemove(feature: Feature): AbstractPass = BY_KEY.values.find(_.removes.contains(feature)).get

  def computeGoal(featuresIn: Set[Feature], goal: String): (Seq[AbstractPass], Set[Feature]) = {
    var features = featuresIn
    var permit: Set[Feature] = BY_KEY(goal).permits
    var unorderedPassesSet: mutable.Set[AbstractPass] = mutable.Set() ++ (features -- permit).map(findPassToRemove)
    var passes: mutable.ArrayBuffer[AbstractPass] = mutable.ArrayBuffer()

    while((unorderedPassesSet.flatMap(_.introduces) -- features).nonEmpty) {
      features ++= unorderedPassesSet.map(_.introduces).reduce(_ ++ _)
      permit = unorderedPassesSet.foldLeft(permit)(_ intersect _.permits)
      unorderedPassesSet = mutable.Set() ++ (features -- permit).map(findPassToRemove)
    }

    val unorderedPasses: ArrayBuffer[AbstractPass] = ArrayBuffer() ++ unorderedPassesSet.toSeq.sortBy(_.description)

    while(unorderedPasses.nonEmpty) {
      // Assume no passes are idempotent, which makes ordering much easier.

      val nextPassResults = unorderedPasses.map {
        case pass if (features -- pass.permits).nonEmpty =>
          Left(s"cannot apply ${pass.key} since it does not permit these features: ${features--pass.permits}'")
        case pass if unorderedPasses.exists(_.introduces.intersect(pass.removes).nonEmpty) =>
          val conflictingPass = unorderedPasses.find(_.introduces.intersect(pass.removes).nonEmpty).get
          Left(s"cannot apply ${pass.key} since ${conflictingPass.key} introduces ${conflictingPass.introduces.intersect(pass.removes)}")
        case pass => Right(pass)
      }

      val nextPass = nextPassResults.collectFirst {
        case Right(pass) => pass
      } match {
        case Some(pass) => pass
        case None =>
          nextPassResults.foreach {
            case Left(error) => Warning(error)
            case _ =>
          }
          Abort("No way to proceed!")
          ???
      }

      unorderedPasses -= nextPass
      passes += nextPass
      passes += BY_KEY("check")
      features = features -- nextPass.removes ++ nextPass.introduces
    }

    (passes, features)
  }

  def doPassesByRainbow(): Unit = {
    val resolve = Passes.BY_KEY.apply("java_resolve")
    val standardize = Passes.BY_KEY.apply("standardize")
    val check = Passes.BY_KEY.apply("check")
    val localCheck = Passes.BY_KEY("local-variable-check")

    Seq(resolve, standardize, check, localCheck).foreach(
      pass => report = pass.apply_pass(report, Array()))

    var goals = Seq.empty[String]

    if(sat_check.get()) goals :+= "sat_check"
    if(check_axioms.get()) goals :+= "check-axioms"
    if(check_defined.get()) goals :+= "check-defined"
    if(check_history.get()) goals :+= "check-history"

    goals :+= "silver"

    val visitor = new RainbowVisitor(report.getOutput)
    report.getOutput.asScala.foreach(_.accept(visitor))

    var features = visitor.features.toSet ++ Set(
      // These are "gated" features: they are (too) hard to detect normally.
      vct.col.features.NotFlattened,
      vct.col.features.BeforeSilverDomains,
      vct.col.features.NullAsOptionValue,
      vct.col.features.NotOptimized,
      vct.col.features.DeclarationsNotLifted,
      vct.col.features.ImplicitLabels,
    ) ++ Set(
      // These are normal features, but need to run always for some reason
      vct.col.features.ScatteredDeclarations, // this pass finds duplicate names.
    )

    var passes = Seq.empty[AbstractPass]

    for(goal <- goals) {
      val (newPasses, newFeatures) = computeGoal(features, goal)
      passes = passes ++ newPasses :+ BY_KEY(goal) :+ BY_KEY("check")
      features = newFeatures
    }

    passes = passes.init
    var featuresIn: Set[Feature] = Set.empty
    var featuresOut: Set[Feature] = Set.empty
    var lastPass: AbstractPass = null

    passes.foreach(pass => {
      Progress("%s", pass.description)

      if(pass.key != "check") {
        featuresIn = Feature.scan(report.getOutput)
        lastPass = pass
      }

      report = pass.apply_pass(report, Array())

      if(pass.key == "check") {
        featuresOut = Feature.scan(report.getOutput)
        val notRemoved = featuresOut.intersect(lastPass.removes)
        val extraIntro = featuresOut -- featuresIn -- lastPass.introduces

        if(notRemoved.nonEmpty) {
          Output("!! Pass %s did not remove %s", lastPass.key, notRemoved)
        }
        if(extraIntro.nonEmpty) {
          Output("!! Pass %s introduced %s", lastPass.key, extraIntro)
        }
      }
    })

    val verdict = if(report.getFatal == 0) "Pass" else "Fail"
    Verdict("The final verdict is %s", verdict)
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
        // doPasses(getPasses)
        doPassesByRainbow()
      }
    } catch {
      case e: HREExitException =>
        exit = e.exit
        Verdict("The final verdict is Error")
      case e: Throwable =>
        DebugException(e)
        Warning("An unexpected error occured in VerCors! " + "Please report an issue at https://github.com/utwente-fmt/vercors/issues/new. " + "You can see the full exception by adding '--debug vct.main.Main' to the flags.")
        Verdict("The final verdict is Error")
    } finally Progress("entire run took %d ms", Long.box(System.currentTimeMillis - wallStart))
    exit
  }
}