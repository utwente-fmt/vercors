package vct.main.options

import hre.config
import hre.config.{BooleanSetting, ChoiceSetting, CollectSetting, IntegerSetting, OptionParser, StringListSetting, StringSetting}

object CommandLineOptions {
  val version = new BooleanSetting(false)
  val helpPasses = new BooleanSetting(false)
  val logLevel = new ChoiceSetting(Array("silent", "abort", "result", "warning", "info", "progress", "debug", "all"), "info")
  val debugFilters = new CollectSetting
  val showBefore = new StringListSetting
  val showAfter = new StringListSetting
  val debugBefore = new CollectSetting
  val debugAfter = new CollectSetting
  val showFile = new StringSetting(null)
  val notifySetting = new BooleanSetting(false)

  val passList = new StringListSetting
  val passListOption: config.Option = passList.getAppendOption("add to the custom list of compilation passes")
  val stopAfter = new StringListSetting
  val stopBeforeBackend = new BooleanSetting(false)
  val stopAfterTypecheck = new BooleanSetting(false)
  val strictInternalConditions = new BooleanSetting(false)
  var inputPaths = Array.empty[String]

  val silver = new StringSetting("silver")

  val checkDefined = new BooleanSetting(false)
  val checkAxioms = new BooleanSetting(false)
  val checkHistory = new BooleanSetting(false)
  val separateChecks = new BooleanSetting(false)
  val sequentialSpec = new BooleanSetting(false)
  val globalWithField = new BooleanSetting(false)
  val noContext = new BooleanSetting(false)
  val guiContext = new BooleanSetting(false)
  val satCheck = new BooleanSetting(true)
  val abruptTerminationViaExceptions = new BooleanSetting(false)
  val triggerGeneration = new IntegerSetting(0)
  val learn = new BooleanSetting(false)

  def addOptions(parser: OptionParser): Unit = {
    parser.add(CommandLineOptions.version.getEnable("Output the current version and exit"), "version")
    parser.add(CommandLineOptions.logLevel.getSetOption("Set the logging level"), "verbosity")
    parser.add(CommandLineOptions.logLevel.getExplicitOption("progress", "Show progress through the passes"), "progress", Char.box('v'))
    parser.add(CommandLineOptions.logLevel.getExplicitOption("silent", "Never output anything"), "silent", Char.box('q'))
    parser.add(CommandLineOptions.debugFilters.getAddOption("Add a class to debug, or specify a line with Class:lineno"), "debug")
    parser.add(CommandLineOptions.silver.getAssign("select Silver backend (silicon/carbon)"), "silver")
    parser.add(CommandLineOptions.silver.getAssign("select Silicon backend", "silicon"), "silicon")
    parser.add(CommandLineOptions.silver.getAssign("select Carbon backend", "carbon"), "carbon")
    parser.add(CommandLineOptions.checkDefined.getEnable("Check if the process-algebraic specification itself satisfies its contract."), "check-defined")
    parser.add(CommandLineOptions.checkAxioms.getEnable("Check if defined processes satisfy their contracts."), "check-axioms")
    parser.add(CommandLineOptions.checkHistory.getEnable("Check if the program correctly implements the process-algebraic specification."), "check-history")
    parser.add(CommandLineOptions.separateChecks.getEnable("validate classes separately"), "separate")
    parser.add(CommandLineOptions.helpPasses.getEnable("print help on available passes"), "help-passes")
    parser.add(CommandLineOptions.sequentialSpec.getEnable("sequential specification instead of concurrent"), "sequential")
    parser.add(CommandLineOptions.passListOption, "passes")
    parser.add(CommandLineOptions.showBefore.getAppendOption("Show source code before given passes"), "show-before")
    parser.add(CommandLineOptions.showAfter.getAppendOption("Show source code after given passes"), "show-after")
    parser.add(CommandLineOptions.showFile.getAssign("redirect show output to files instead of stdout"), "save-show")
    parser.add(CommandLineOptions.debugBefore.getAddOption("Dump the COL AST before a pass is run"), "debug-before")
    parser.add(CommandLineOptions.debugAfter.getAddOption("Dump the COL AST after a pass is run"), "debug-after")
    parser.add(CommandLineOptions.notifySetting.getEnable("Send a system notification upon completion"), "notify")
    parser.add(CommandLineOptions.stopAfter.getAppendOption("Stop after given passes"), "stop-after")
    parser.add(CommandLineOptions.stopBeforeBackend.getEnable("Only do parsing, typechecking, and AST transformations. Do not do verification with the backend."), "stop-before-backend")
    parser.add(CommandLineOptions.stopAfterTypecheck.getEnable("Only do parsing and typechecking. Do not apply AST transformations, nor verification with the backend."), "stop-after-typecheck")
    parser.add(CommandLineOptions.strictInternalConditions.getEnable("Enable strict internal checks for AST conditions (expert option)"), "strict-internal")
    parser.add(CommandLineOptions.globalWithField.getEnable("Encode global access with a field rather than a parameter. (expert option)"), "global-with-field")
    parser.add(CommandLineOptions.noContext.getEnable("disable printing the context of errors"), "no-context")
    parser.add(CommandLineOptions.guiContext.getEnable("enable the gui extension of the context"), "gui")
    parser.add(CommandLineOptions.satCheck.getDisable("Disable checking if method pre-conditions are satisfiable"), "disable-sat")
    parser.add(CommandLineOptions.abruptTerminationViaExceptions.getEnable("Force compilation of abrupt termination to exceptions"), "at-via-exceptions")
    parser.add(CommandLineOptions.triggerGeneration.getOptionalAssign("Try to simplify universal quantifiers and generate triggers for them."), "triggers")
    parser.add(CommandLineOptions.learn.getEnable("Learn unit times for AST nodes."), "learn")
  }

}
