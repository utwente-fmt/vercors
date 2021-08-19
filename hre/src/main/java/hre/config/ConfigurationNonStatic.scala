package hre.config

import hre.ast.FileContext
import hre.config

import java.nio.file.Path
import java.util
import java.util.Hashtable

class ConfigurationNonStatic {

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

  /**
    * Switch behavior of witness encoding.
    */
  val witness_constructors=new BooleanSetting(true)

  /**
    * Global options for controlling the deletion of temporary files.
    */
  val keep_temp_files=new BooleanSetting(false)
  /**
    * Global options for increasing the detail used in error messages.
    * The idea is that normal error messages are independent of the
    * back-end used, while detailed messages may contain details which
    * are specific to a back-end.
    */
  val detailed_errors=new BooleanSetting(false)

  /**
    * Set the name of the file that is fed into the back-end verifier.
    * The file is kept after the verification.
    */
  val backend_file=new StringSetting(null)

  /**
    * Control type checking in the PVL parser.
    * By default type checking is enabled, but for multiple file input
    * it must often be disabled as the PVL type checker does not consider libraries.
    */
  val pvl_type_check=new BooleanSetting(true)

  /**
    * When a kernel is a single group, some important simplifications can be performed.
    * Thus we have this option that tells the tools to assume gsize==tcount.
    */
  val assume_single_group=new BooleanSetting(false)

  /**
    * This setting which is true by default controls if all resource
    * are automatically revoked with every kernel barrier.
    */
  val auto_barrier=new BooleanSetting(true)

  /**
    * Enable the resource check during kernel verification.
    */
  val enable_resource_check=new BooleanSetting(true)

  /**
    * Enable post check during kernel verification.
    */
  val enable_post_check=new BooleanSetting(true)

  /**
    * The include path passed to the C pre processor.
    */
  val cpp_include_path=new StringListSetting()

  /**
    * The definitions passed to the C pre processor.
    */
  val cpp_defines=new StringListSetting()

  /**
    * The command that invokes the C pre processor.
    */
  val cpp_command=new StringSetting("clang -C -E")

  val debugBackend = new BooleanSetting(false)
  val ansi = new BooleanSetting(false)

  /**
    * The option for veymont decomposition
    */
  val veymont_file =new StringSetting(null)


  val profiling=new IntegerSetting(1000)
  val profiling_option: Option =profiling.getOptionalAssign("Enable profiling")

  val skip=new StringListSetting()

  val fileContexts: util.Hashtable[Path, FileContext] = new util.Hashtable[Path, FileContext]

  /**
    * Add the VCT library options to the given option parser.
    * @param parser Option parser.
    */
  def addOptions(parser: OptionParser): Unit = {
    parser.add(keep_temp_files.getEnable("keep temporary files"),"keep");
    parser.add(detailed_errors.getEnable("produce detailed error messages"),"detail");
    parser.add(backend_file.getAssign("filename for storing the back-end input"),"encoded");
    parser.add(assume_single_group.getEnable("enable single group assumptions"),"single-group");
    parser.add(auto_barrier.getDisable("Disable automatic permission revokation for barriers"),"disable-auto-barrier");
    parser.add(enable_resource_check.getDisable("disable barrier resource check during kernel verification"),"disable-resource-check");
    parser.add(enable_post_check.getDisable("disable barrier post check during kernel verification"),"disable-post-check");
    parser.add(witness_constructors.getEnable("use constructors for witnesses"),"witness-constructors");
    parser.add(witness_constructors.getDisable("inline constructors for witnesses"),"witness-inline");
    parser.add(cpp_command.getAssign("set the C Pre Processor command"),"cpp");
    parser.add(cpp_include_path.getAppendOption("add to the CPP include path"),'I',"include")
    parser.add(cpp_defines.getAppendOption("add to the CPP defined variables"),'D');
    parser.add(profiling_option, "profile");
    parser.add(skip.getAppendOption("comma separated list of methods that may be skipped during verification"),"skip");
    parser.add(debugBackend.getEnable("Instruct the selected backend to output debug information"), "debug-backend");
    parser.add(ansi.getEnable("Add pretty-printing features for terminals supporting ANSI escape sequences"), "ansi");
    parser.add(veymont_file.getAssign(
      "VeyMont decomposes the global program from the input files into several local programs that can be executed in parallel. " +
        "The program from the input files has to adhere to the syntax of a 'global program'. Syntax violations result in VeyMont Fail messages. " +
        "The decomposition preserves the behaviour of the global program. " +
        "This implies that all functional properties proven (with VerCors) for the global program also hold for the local program. " +
        "Memory and thread safety can be checked by running VerCors on the file produced by VeyMont. " +
        "Also, both global programs and their decomposed local programs are deadlock-free by construction." +
        "For more information on VeyMont, please check the VerCors Wiki."),"veymont");
    parser.add(version.getEnable("Output the current version and exit"), "version")
    parser.add(logLevel.getSetOption("Set the logging level"), "verbosity")
    parser.add(logLevel.getExplicitOption("progress", "Show progress through the passes"), "progress", Char.box('v'))
    parser.add(logLevel.getExplicitOption("silent", "Never output anything"), "silent", Char.box('q'))
    parser.add(debugFilters.getAddOption("Add a class to debug, or specify a line with Class:lineno"), "debug")
    parser.add(silver.getAssign("select Silver backend (silicon/carbon)"), "silver")
    parser.add(silver.getAssign("select Silicon backend", "silicon"), "silicon")
    parser.add(silver.getAssign("select Carbon backend", "carbon"), "carbon")
    parser.add(checkDefined.getEnable("Check if the process-algebraic specification itself satisfies its contract."), "check-defined")
    parser.add(checkAxioms.getEnable("Check if defined processes satisfy their contracts."), "check-axioms")
    parser.add(checkHistory.getEnable("Check if the program correctly implements the process-algebraic specification."), "check-history")
    parser.add(separateChecks.getEnable("validate classes separately"), "separate")
    parser.add(helpPasses.getEnable("print help on available passes"), "help-passes")
    parser.add(sequentialSpec.getEnable("sequential specification instead of concurrent"), "sequential")
    parser.add(passListOption, "passes")
    parser.add(showBefore.getAppendOption("Show source code before given passes"), "show-before")
    parser.add(showAfter.getAppendOption("Show source code after given passes"), "show-after")
    parser.add(showFile.getAssign("redirect show output to files instead of stdout"), "save-show")
    parser.add(debugBefore.getAddOption("Dump the COL AST before a pass is run"), "debug-before")
    parser.add(debugAfter.getAddOption("Dump the COL AST after a pass is run"), "debug-after")
    parser.add(notifySetting.getEnable("Send a system notification upon completion"), "notify")
    parser.add(stopAfter.getAppendOption("Stop after given passes"), "stop-after")
    parser.add(stopBeforeBackend.getEnable("Only do parsing, typechecking, and AST transformations. Do not do verification with the backend."), "stop-before-backend")
    parser.add(stopAfterTypecheck.getEnable("Only do parsing and typechecking. Do not apply AST transformations, nor verification with the backend."), "stop-after-typecheck")
    parser.add(strictInternalConditions.getEnable("Enable strict internal checks for AST conditions (expert option)"), "strict-internal")
    parser.add(globalWithField.getEnable("Encode global access with a field rather than a parameter. (expert option)"), "global-with-field")
    parser.add(noContext.getEnable("disable printing the context of errors"), "no-context")
    parser.add(guiContext.getEnable("enable the gui extension of the context"), "gui")
    parser.add(satCheck.getDisable("Disable checking if method pre-conditions are satisfiable"), "disable-sat")
    parser.add(abruptTerminationViaExceptions.getEnable("Force compilation of abrupt termination to exceptions"), "at-via-exceptions")
    parser.add(triggerGeneration.getOptionalAssign("Try to simplify universal quantifiers and generate triggers for them."), "triggers")
    parser.add(learn.getEnable("Learn unit times for AST nodes."), "learn")
  }
}
