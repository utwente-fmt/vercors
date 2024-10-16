package vct.options

import hre.log.Verbosity
import scopt.OParser
import scopt.Read._
import vct.main.BuildInfo
import vct.main.stages.Parsing.Language
import vct.rewrite.veymont.verification.EncodePermissionStratification.{
  Mode => PermissionStratificationMode
}
import vct.options.types._
import vct.resources.Resources

import java.nio.file.{Path, Paths}
import scala.collection.mutable
import scala.reflect.ClassTag

case object Options {
  private val builder = OParser.builder[Options]

  def parser(hide: Boolean = true): OParser[Unit, Options] =
    constructParser(hide)._1

  def constructParser(
      hide: Boolean
  ): (OParser[Unit, Options], Map[String, ClassTag[_]]) = {
    import builder._

    implicit class Hideable[A, C](opt: OParser[A, C]) {
      def maybeHidden(): OParser[A, C] =
        if (hide)
          opt.hidden()
        else
          opt
    }

    val tags: mutable.Map[String, ClassTag[_]] = mutable.Map()

    def opt[T: scopt.Read](
        name: String
    )(implicit tag: ClassTag[T]): OParser[T, Options] = {
      val parser = builder.opt[T](name)
      tags(parser.toList.head.name) = tag
      parser
    }

    import vct.options.types.Backend.read
    implicit val readLanguage: scopt.Read[Language] = ReadLanguage.read
    implicit val readPermissionStratificationMode
        : scopt.Read[PermissionStratificationMode] =
      ReadPermissionStratificationMode.read
    import ReadEnum.readVerbosity

    implicit val readPathOrStd: scopt.Read[PathOrStd] = scopt.Read.reads {
      case "-" => PathOrStd.StdInOrOut
      case other => PathOrStd.Path(Paths.get(other))
    }

    implicit val readPath: scopt.Read[Path] = scopt.Read.reads(Paths.get(_))

    val parser = OParser.sequence(
      programName(BuildInfo.name),
      head(BuildInfo.name, BuildInfo.version),
      opt[Unit]("help").abbr("h").action((_, c) => c.copy(help = true))
        .text("Prints this usage text"),
      opt[Unit]("help-hidden").action((_, c) => c.copy(showHidden = true)).text(
        "Show hidden options (intended for VerCors experts, proceed with caution!)"
      ),
      version("version").text("Prints version and build information"),
      opt[Unit]("help-passes").action((_, c) =>
        c.copy(mode = Mode.HelpVerifyPasses)
      ).text("Lists the pass keys available for options that take a pass key."),
      opt[Unit]("quiet").abbr("q").action((_, c) =>
        c.copy(logLevels =
          c.logLevels ++
            Seq(("vct", Verbosity.Error), ("viper.api", Verbosity.Error))
        )
      ).text("Instruct VerCors to only log errors."),
      opt[Unit]("verbose").abbr("v").action((_, c) =>
        c.copy(logLevels =
          c.logLevels ++
            Seq(("vct", Verbosity.Debug), ("viper.api", Verbosity.Debug))
        )
      ).text("Instruct VerCors to output debug information"),
      opt[Unit]("progress").abbr("p").action((_, c) => c.copy(progress = true))
        .text("Print progress information, even if stdout is not a tty."),
      opt[Unit]("profile").action((_, c) => c.copy(profile = true)).text(
        "Output profiling information in the current directory in the pprof format (https://github.com/google/pprof)"
      ),
      opt[Unit]("watch").abbr("w").action((_, c) => c.copy(watch = true)).text(
        "Run VerCors in an infinite loop, waiting for external changes between each run."
      ),
      opt[(String, Verbosity)]("dev-log-verbosity").unbounded().maybeHidden()
        .keyValueName("<loggerKey>", "<verbosity>")
        .action((tup, c) => c.copy(logLevels = c.logLevels :+ tup))
        .text("Set the log level for a custom logger key"),
      note(""),
      note("Verification Mode"),
      opt[Unit]("verify").action((_, c) => c.copy(mode = Mode.Verify)).text(
        "Enable verification mode: instruct VerCors to verify the given files (default)"
      ),
      opt[Unit]("more").abbr("m").action((_, c) => c.copy(more = true))
        .text("Always print the maximum amount of information about errors."),
      opt[Language]("lang").valueName(ReadLanguage.valueName)
        .action((lang, c) => c.copy(language = Some(lang))).text(
          "Do not detect the language from the file extension, but force a specific language parser for all files"
        ),
      opt[Backend]("backend").valueName(Backend.valueName)
        .action((backend, c) => c.copy(backend = backend))
        .text("Set the backend to verify with (default: silicon)"),
      opt[Path]("backend-file-base").valueName("<path>")
        .action((backendFile, c) => c.copy(backendFile = Some(backendFile)))
        .text(
          "In addition to verification, output the resulting ASTs for the backend to files, appended with -<number>.<extension>"
        ),
      opt[Unit]("backend-debug").action((_, c) =>
        c.copy(logLevels = c.logLevels :+ ("viper", Verbosity.Debug))
      ).text(
        "Instruct the backend to print as much debugging information as possible"
      ),
      opt[(String, PathOrStd)]("output-after-pass").unbounded()
        .keyValueName("<pass>", "<path>").action((output, c) =>
          c.copy(outputAfterPass = c.outputAfterPass ++ Map(output))
        ).text("Print the AST after a pass key"),
      opt[(String, PathOrStd)]("output-before-pass").unbounded()
        .keyValueName("<pass>", "<path>").action((output, c) =>
          c.copy(outputBeforePass = c.outputBeforePass ++ Map(output))
        ).text("Print the AST before a pass key"),
      opt[Unit]("trace-col").action((_, c) =>
        c.copy(outputIntermediatePrograms = Some(Paths.get("tmp", "cols")))
      ).text("Writes all intermediate ASTs, labeled by pass, to tmp/cols/"),
      opt[Path]("trace-col-in")
        .action((p, c) => c.copy(outputIntermediatePrograms = Some(p))).text(
          "Writes all intermediate ASTs, labeled by pass, to a given folder"
        ),
      opt[String]("backend-option").unbounded().keyName("<option>,...")
        .action((opt, c) => c.copy(backendFlags = c.backendFlags :+ opt))
        .text("Provide custom flags to Viper"),
      opt[Unit]("skip-backend").action((_, c) => c.copy(skipBackend = true))
        .text(
          "Stop VerCors successfully before the backend is used to verify the program"
        ),
      opt[Unit]("skip-translation")
        .action((_, c) => c.copy(skipTranslation = true)).text(
          "Stop VerCors successfully immediately after the file is parsed and resolved, and do no further processing"
        ),
      opt[String]("skip-translation-after").valueName("<pass>")
        .action((pass, c) => c.copy(skipTranslationAfter = Some(pass))).text(
          "Stop VerCors successfully after executing the transformation pass with the supplied key"
        ),
      opt[String]("skip-pass").unbounded().valueName("<pass>")
        .action((pass, c) => c.copy(skipPass = c.skipPass + pass))
        .text("Skip the passes that have the supplied keys"),
      opt[Int]("silicon-print-quantifier-stats").valueName("<amount>").action(
        (amount, c) => c.copy(siliconPrintQuantifierStats = Some(amount))
      ).text(
        "Print quantifier instantiation statistics from Z3 via silicon, every <amount> instantiations, every 5 seconds. Implies --dev-silicon-num-verifiers 1"
      ),
      opt[Unit]("silicon-quiet").action((_, c) =>
        c.copy(
          devSiliconReportOnNoProgress = false,
          devSiliconTraceBranchConditions = false,
          devSiliconBranchConditionReportInterval = None,
        )
      ).text("Disable various diagnostics of the silicon backend."),
      opt[PathOrStd]("bip-report-file").valueName("<path>")
        .action((p, c) => c.copy(bipReportFile = Some(p))).text(
          "Write JavaBIP verification report to file, or standard out if \"-\" is used"
        ),
      opt[Unit]("no-infer-heap-context-into-frame")
        .action((_, c) => c.copy(inferHeapContextIntoFrame = false)).text(
          "Disables smart inference of contextual heap into frame statements using `forperm`"
        ),
      opt[Unit]("dev-parsing-ambiguities").maybeHidden()
        .action((_, c) => c.copy(devParserReportAmbiguities = true))
        .text("Report instances of ambiguities in the parsed inputs"),
      opt[Unit]("dev-parsing-sensitivities").maybeHidden()
        .action((_, c) => c.copy(devParserReportContextSensitivities = true))
        .text("Report instances of context sensitivities in the parsed inputs"),
      opt[Unit]("dev-abrupt-exc").maybeHidden()
        .action((_, c) => c.copy(devAbruptExc = true)).text(
          "Encode all abrupt control flow using exception, even when not necessary"
        ),
      opt[Unit]("dev-no-sat").maybeHidden()
        .action((_, c) => c.copy(devCheckSat = false))
        .text("Do not check the satisfiability of contracts in the input"),
      opt[String]("dev-simplify-debug-in").unbounded().maybeHidden()
        .valueName("<declaration>").action((decl, c) =>
          c.copy(devSimplifyDebugIn = c.devSimplifyDebugIn :+ decl)
        ).text(
          "Debug simplifications below a declaration preferredName (recommended to inspect --output-before-pass simplify=-)"
        ),
      opt[Unit]("dev-simplify-debug-match").maybeHidden()
        .action((_, c) => c.copy(devSimplifyDebugMatch = true))
        .text("Debug matched expressions in simplifications"),
      opt[Unit]("dev-simplify-debug-match-long").maybeHidden()
        .action((_, c) => c.copy(devSimplifyDebugMatchShort = false))
        .text("Use long form to print matched expressions in simplifications"),
      opt[Unit]("dev-simplify-debug-no-match").maybeHidden()
        .action((_, c) => c.copy(devSimplifyDebugNoMatch = true))
        .text("Debug expressions that do not match in simplifications"),
      opt[String]("dev-simplify-debug-filter-input-kind").maybeHidden()
        .action((kind, c) =>
          c.copy(devSimplifyDebugFilterInputKind = Some(kind))
        ).text("Debug only expressions of a certain kind by simple class name"),
      opt[String]("dev-simplify-debug-filter-rule").maybeHidden()
        .action((rule, c) => c.copy(devSimplifyDebugFilterRule = Some(rule)))
        .text("Debug only applications of a particular rule, by name"),
      opt[Unit]("dev-cache").maybeHidden()
        .action((_, c) => c.copy(devCache = true))
        .text("Cache verification results (slow, experimental)"),
      opt[Unit]("dev-split-verification-by-procedure").maybeHidden()
        .action((_, c) => c.copy(devSplitVerificationByProcedure = true)).text(
          "Invoke separate instances of the backend for each procedure at the end of the rewrite chain (slow, experimental)"
        ),
      opt[Int]("dev-silicon-num-verifiers").maybeHidden()
        .action((amount, c) => c.copy(devSiliconNumVerifiers = Some(amount)))
        .text(
          "Indicate the number of verifiers for silicon to use. In practice the number of silicon threads equals this number + 1"
        ),
      opt[Int]("dev-silicon-branch-condition-report-interval").maybeHidden()
        .action((interval, c) =>
          c.copy(devSiliconBranchConditionReportInterval = Some(interval))
        ).text(
          "The interval of branch trace records at which to report the current path condition"
        ),
      opt[Unit]("dev-silicon-no-branch-condition-report").maybeHidden()
        .action((_, c) =>
          c.copy(devSiliconBranchConditionReportInterval = None)
        ).text("Do not report the current branch condition at an interval"),
      opt[Unit]("dev-silicon-trace-branch-conditions").maybeHidden()
        .action((_, c) =>
          c.copy(
            devSiliconTraceBranchConditions = true,
            devSiliconBranchConditionReportInterval = None,
          )
        ).text("Trace all branch condition records, rendered as a tree"),
      opt[Unit]("dev-silicon-no-report-on-no-progress").maybeHidden()
        .action((_, c) => c.copy(devSiliconReportOnNoProgress = false)).text(
          "Do not report the current state of silicon when no progress is made for some time"
        ),
      opt[Int]("dev-assert-timeout").maybeHidden().action((amount, c) =>
        c.copy(devSiliconAssertTimeout = amount)
      ).text(
        "Indicate, in seconds, the timeout value for a single assert statement. If the verification gets stuck " +
          "on a single SMT check for longer than this timeout, the verification will fail."
      ),
      opt[Int]("dev-total-timeout").maybeHidden().action((amount, c) =>
        c.copy(devSiliconTotalTimeout = amount)
      ).text(
        "Indicate, in seconds, the timeout value for the backend verification. If the verification gets stuck " +
          "for longer than this timeout, the verification will timeout."
      ),
      opt[Unit]("dev-unsafe-optimization").maybeHidden().action((_, c) =>
        c.copy(devUnsafeOptimization = true, devCheckSat = false)
      ).text(
        "Optimizes runtime at the cost of progress logging and readability of error messages. Implies --dev-no-sat."
      ),
      opt[Path]("dev-silicon-z3-log-file").maybeHidden()
        .action((p, c) => c.copy(devSiliconZ3LogFile = Some(p)))
        .text("Path for z3 to write smt2 log file to"),
      opt[Path]("dev-carbon-boogie-log-file").maybeHidden()
        .action((p, c) => c.copy(devCarbonBoogieLogFile = Some(p)))
        .text("Path for boogie to write smt2 log file to"),
      opt[Path]("dev-viper-prover-log-file").maybeHidden()
        .action((p, c) => c.copy(devViperProverLogFile = Some(p))).text(
          "Path for viper to write boogie or smt2 input file to, depending on selected backend"
        ),
      opt[Map[String, String]]("c-define").valueName("<macro>=<defn>,...")
        .action((defines, c) => c.copy(cDefine = defines))
        .text("Pass -D options to the C preprocessor"),
      opt[Seq[PathOrStd]]("paths-simplify").valueName("<simplify.pvl>,...")
        .action((paths, c) => c.copy(simplifyPaths = paths)).text(
          "Specify a chain of files to use that contain axiomatic simplification rules"
        ),
      opt[Seq[PathOrStd]]("paths-simplify-after-relations")
        .valueName("<simplify.pvl>,...")
        .action((paths, c) => c.copy(simplifyPathsAfterRelations = paths)).text(
          "Specify a chain of files to use the contain axiomatic simplification rules, which will be applied after quantified integer relations are simplified"
        ),
      opt[Path]("path-adt").valueName("<path>")
        .action((path, c) => c.copy(adtPath = path)).text(
          "Use a custom directory that contains definitions for all internal types encoded as axiomatic datatypes (array, option, any, etc.)"
        ),
      opt[Path]("path-cc").valueName("<path>")
        .action((path, c) => c.copy(cc = path))
        .text("Set the C compiler to use for preprocessing"),
      opt[Path]("path-c-system").valueName("<path>")
        .action((path, c) => c.copy(cIncludePath = path))
        .text("Set the include path for system headers (-isystem)"),
      opt[Unit]("no-std-class-path").action((_, c) =>
        c.copy(classPath = c.classPath.collect {
          case ClassPathEntry.SourcePath(p) => ClassPathEntry.SourcePath(p)
        })
      ).text(
        "Remove the @jre (the default path to specified classes in the java runtime) and @source (the sources root computed via the package entry of submitted sources) entry"
      ),
      opt[ClassPathEntry]("class-path").valueName("<path>|@jre|@source")
        .unbounded().action((cp, c) => c.copy(classPath = c.classPath :+ cp))
        .text("Add an entry to the sources class path"),
      opt[Path]("path-z3").valueName("<path>")
        .action((path, c) => c.copy(z3Path = path))
        .text("Set the location of the z3 binary"),
      opt[Path]("path-boogie").valueName("<path>")
        .action((path, c) => c.copy(boogiePath = path))
        .text("Set the location of the boogie binary"),
      opt[Path]("path-c-preprocessor").valueName("<path>")
        .action((path, c) => c.copy(cPreprocessorPath = path))
        .text("Set the location of the C preprocessor binary"),
      opt[Unit]("generate-permissions")
        .action((_, c) => c.copy(generatePermissions = true)).text(
          "Generates permissions for the entire program using a syntax-driven single-owner policy"
        ),
      note(""),
      note("VeyMont Mode"),
      opt[Unit]("veymont").action((_, c) => c.copy(mode = Mode.VeyMont)).text(
        "Enable VeyMont mode: decompose the global program from the input files into several local programs that can be executed in parallel"
      ).children(
        opt[Unit]("choreography").abbr("chor").action((_, c) =>
          c.copy(veymontSkipImplementationVerification = true)
        ).text("Only perform verification of the choreography."),
        opt[Unit]("implementation").abbr("impl")
          .action((_, c) => c.copy(veymontSkipChoreographyVerification = true))
          .text("Only perform verification of the generated implementation."),
        opt[Unit]("generate").abbr("gen").action((_, c) =>
          c.copy(
            veymontSkipChoreographyVerification = true,
            veymontSkipImplementationVerification = true,
          )
        ).text(
          "Only generate an implementation, and skip the choreography and implementation verification steps"
        ),
        opt[Path]("veymont-output").valueName("<path>")
          .action((path, c) => c.copy(veymontOutput = Some(path))).text(
            "Indicates output path for generated implementation. The extension decides the output language: `.pvl` is PVL, `.java` is Java."
          ),
        opt[Path]("veymont-resource-path").valueName("<path>")
          .action((path, c) => c.copy(veymontResourcePath = path)),
        opt[Unit]("veymont-skip-choreography-verification")
          .action((_, c) => c.copy(veymontSkipChoreographyVerification = true))
          .text(
            "Do not verify choreographies, skipping to implementation generation & verification immediately"
          ),
        opt[Unit]("veymont-skip-implementation-verification").action((_, c) =>
          c.copy(veymontSkipImplementationVerification = true)
        ).text("Do not verify generated implementation"),
        opt[PermissionStratificationMode]("veymont-ps").action((mode, c) =>
          c.copy(veymontPermissionStratificationMode = mode)
        ).text(
          "Specifies the implementation of stratified permissions to use. Possible options: wrap (default), inline and none."
        ),
      ),
      opt[Unit]("veymont-no-branch-unanimity").maybeHidden()
        .action((_, c) => c.copy(veymontBranchUnanimity = false)).text(
          "Disables generation of the branch unanimity check encoded by VeyMont, which ensures that endpoints cannot disagree about which branch to take. This check cannot always be computed, but if it can, it saves the user from having to prove this informally."
        ),
      note(""),
      note("VeSUV Mode"),
      opt[Unit]("vesuv").action((_, c) => c.copy(mode = Mode.VeSUV)).text(
        "Enable VeSUV mode: transform SystemC designs to PVL to be deductively verified"
      ).children(
        opt[Path]("vesuv-output").required().valueName("<path>")
          .action((path, c) => c.copy(vesuvOutput = path))
          .text("Output file for the result of the transformation"),
        opt[Unit]("generate-rasi")
          .action((_, c) => c.copy(vesuvGenerateRasi = true)).text(
            "Instead of transforming a SystemC design to PVL, generate a global invariant for a PVL program"
          ).children(
            opt[Unit]("rasi-graph-output")
              .action((_, c) => c.copy(vesuvRasiTest = true)).text(
                "Output RASI graph to test the algorithm rather than outputting the invariant"
              ),
            opt[Seq[String]]("rasi-vars").valueName("<var1>,...")
              .action((vars, c) => c.copy(vesuvRasiVariables = Some(vars)))
              .text(
                "[WIP] Preliminary selection mechanism for RASI variables; might be replaced later"
              ),
            opt[Seq[String]]("split-rasi").valueName("<var1>,...")
              .action((vars, c) => c.copy(vesuvRasiSplitVariables = Some(vars)))
              .text(
                "[WIP] Preliminary selection mechanism for localizing the RASI based on certain variables; might be changed later"
              ),
          ),
      ),
      note(""),
      note("Control flow graph"),
      opt[Unit]("build-cfg").action((_, c) => c.copy(mode = Mode.CFG)).text(
        "Instead of verifying a program, build its control flow graph for further analysis"
      ).children(
        opt[Path]("cfg-output").required().valueName("<path>")
          .action((path, c) => c.copy(cfgOutput = path))
          .text("Output file for the control flow graph in .dot format")
      ),
      note(""),
      note("Compile mode"),
      opt[Unit]("compile").action((_, c) => c.copy(mode = Mode.Compile)).text(
        "Compiles PVL to Java. Currently only supported for the imperative fragment of PVL."
      ).children(
        opt[Path]("compile-output").valueName("<path>")
          .action((path, c) => c.copy(compileOutput = Some(path)))
          .text("Output Java file")
      ),
      note(""),
      note("Patcher mode"),
      opt[Unit]("patcher").action((_, c) => c.copy(mode = Mode.Patcher)).text(
        "Patches a file given a patch in the custom VerCors patch format."
      ).children(
        opt[Path]("patch-file").valueName("<path>").required()
          .action((path, c) => c.copy(patchFile = path))
          .text("Path to patch file to apply"),
        opt[Path]("patch-output").valueName("<path>").required().action(
          (path, c) => c.copy(patchOutput = path)
        ).text(
          "Output path. If the patcher is given only one input, this is interpeted as a file destination." +
            " " +
            "If the patcher is given multiple inputs, this is interpreted as a directory path."
        ),
      ),
      note(""),
      note(""),
      arg[PathOrStd]("<path>...").unbounded().optional()
        .action((path, c) => c.copy(inputs = c.inputs :+ path))
        .text("List of input files to process"),
    )

    (parser, tags.toMap)
  }

  def parse(args: Array[String]): Option[Options] =
    OParser.parse(parser(), args, Options())
}

/** Stores all command line options values, nicely parsed.
  *
  * Components of VerCors, in particular rewrite passes, must not be passed this
  * object directly. Instead, duplicate the option as a parameter to e.g. the
  * rewrite pass class, then pass in the option in
  * [[vct.main.stages.SilverTransformation]].
  */
case class Options(
    help: Boolean = false,
    showHidden: Boolean = false,
    mode: Mode = Mode.Verify,
    inputs: Seq[PathOrStd] = Nil,
    logLevels: Seq[(String, Verbosity)] = Seq(
      ("vct", Verbosity.Info),
      ("viper", Verbosity.Off),
      ("viper.api", Verbosity.Info),
    ),
    progress: Boolean = false,
    profile: Boolean = false,
    watch: Boolean = false,
    more: Boolean = false,

    // Verify Options
    language: Option[Language] = None,
    backend: Backend = Backend.Silicon,
    backendFile: Option[Path] = None,
    outputAfterPass: Map[String, PathOrStd] = Map.empty,
    outputBeforePass: Map[String, PathOrStd] = Map.empty,
    outputIntermediatePrograms: Option[Path] = None,
    backendFlags: Seq[String] = Nil,
    skipBackend: Boolean = false,
    skipTranslation: Boolean = false,
    skipTranslationAfter: Option[String] = None,
    skipPass: Set[String] = Set.empty,
    cDefine: Map[String, String] = Map.empty,
    simplifyPaths: Seq[PathOrStd] = Seq("pushin", "simplify")
      .map(name => PathOrStd.Path(Resources.getSimplificationPath(name))),
    simplifyPathsAfterRelations: Seq[PathOrStd] = Seq("simplify")
      .map(name => PathOrStd.Path(Resources.getSimplificationPath(name))),
    adtPath: Path = Resources.getAdtPath,
    cc: Path = Resources.getCcPath,
    cIncludePath: Path = Resources.getCIncludePath,
    ccpp: Path = Resources.getCPPcPath,
    cppIncludePath: Path = Resources.getCPPIncludePath,
    classPath: Seq[ClassPathEntry] = Seq(
      ClassPathEntry.DefaultJre,
      ClassPathEntry.SourcePackageRoot,
    ),
    z3Path: Path = viper.api.Resources.getZ3Path,
    boogiePath: Path = viper.api.Resources.getBoogiePath,
    cPreprocessorPath: Path = Resources.getCcPath,
    siliconPrintQuantifierStats: Option[Int] = None,
    bipReportFile: Option[PathOrStd] = None,
    inferHeapContextIntoFrame: Boolean = true,
    generatePermissions: Boolean = false,

    // Verify options - hidden
    devParserReportAmbiguities: Boolean = false,
    devParserReportContextSensitivities: Boolean = false,
    devAbruptExc: Boolean = false,
    devCheckSat: Boolean = true,
    devSimplifyDebugIn: Seq[String] = Nil,
    devSimplifyDebugMatch: Boolean = false,
    devSimplifyDebugMatchShort: Boolean = true,
    devSimplifyDebugNoMatch: Boolean = false,
    devSimplifyDebugFilterInputKind: Option[String] = None,
    devSimplifyDebugFilterRule: Option[String] = None,
    devCache: Boolean = false,
    devSplitVerificationByProcedure: Boolean = false,
    devSiliconNumVerifiers: Option[Int] = None,
    devSiliconZ3LogFile: Option[Path] = None,
    devSiliconAssertTimeout: Int = 30,
    devSiliconTotalTimeout: Int = 0,
    devSiliconReportOnNoProgress: Boolean = true,
    devSiliconBranchConditionReportInterval: Option[Int] = Some(1000),
    devSiliconTraceBranchConditions: Boolean = false,
    devCarbonBoogieLogFile: Option[Path] = None,
    devViperProverLogFile: Option[Path] = None,
    devUnsafeOptimization: Boolean = false,

    // VeyMont options
    veymontOutput: Option[Path] = None,
    veymontResourcePath: Path = Resources.getVeymontPath,
    veymontBranchUnanimity: Boolean = true,
    // Stratified permission settings
    veymontPermissionStratificationMode: PermissionStratificationMode =
      PermissionStratificationMode.Wrap,
    veymontSkipChoreographyVerification: Boolean = false,
    veymontSkipImplementationVerification: Boolean = false,

    // VeSUV options
    vesuvOutput: Path = null,
    vesuvGenerateRasi: Boolean = false,
    vesuvRasiTest: Boolean = false,
    vesuvRasiVariables: Option[Seq[String]] = None,
    vesuvRasiSplitVariables: Option[Seq[String]] = None,

    // Control flow graph options
    cfgOutput: Path = null,

    // Compile options
    compileOutput: Option[Path] = None,

    // Patch options
    patchFile: Path = null,
    patchOutput: Path = null,
) {
  def getParserDebugOptions: vct.parsers.debug.DebugOptions =
    vct.parsers.debug.DebugOptions(
      reportAmbiguities = devParserReportAmbiguities,
      reportContextSensitivity = devParserReportContextSensitivities,
    )
}
