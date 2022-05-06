package vct.options

import scopt.OParser
import scopt.Read._
import vct.col.newrewrite.Minimize
import vct.main.BuildInfo
import vct.parsers.Language
import vct.resources.Resources

import java.nio.file.{Path, Paths}

case object Options {
  private val builder = OParser.builder[Options]

  private val parser = {
    import builder._

    implicit val readBackend: scopt.Read[Backend] =
      scopt.Read.reads {
        case "silicon" => Backend.Silicon
        case "carbon" => Backend.Carbon
      }

    implicit val readLanguage: scopt.Read[Language] =
      scopt.Read.reads {
        case "java" => Language.Java
        case "c" => Language.C
        case "i" => Language.InterpretedC
        case "pvl" => Language.PVL
        case "silver" => Language.Silver
      }

    implicit val readPathOrStd: scopt.Read[PathOrStd] =
      scopt.Read.reads {
        case "-" => PathOrStd.StdInOrOut
        case other => PathOrStd.Path(Paths.get(other))
      }

    implicit val readPath: scopt.Read[Path] = scopt.Read.reads(Paths.get(_))

    implicit val readVerbosity: scopt.Read[Verbosity] =
      scopt.Read.reads {
        case "off" => Verbosity.Off
        case "error" => Verbosity.Error
        case "warning" => Verbosity.Warning
        case "info" => Verbosity.Info
        case "debug" => Verbosity.Debug
        case "trace" => Verbosity.Trace
        case "all" => Verbosity.All
      }

    implicit val readMinimizeTarget: scopt.Read[MinimizeName] =
      scopt.Read.reads(MinimizeName.parse)

    implicit val readMinimizeMode: scopt.Read[MinimizeMode] =
      scopt.Read.reads {
        case "focus" => MinimizeMode.Focus
        case "ignore" => MinimizeMode.Ignore
      }

    OParser.sequence(
      programName(BuildInfo.name),
      head(BuildInfo.name, BuildInfo.version),

      help("help").abbr("h").text("Prints this usage text"),
      version("version").text("Prints version and build information"),
      opt[Unit]("help-passes")
        .action((_, c) => c.copy(mode = Mode.HelpVerifyPasses))
        .text("Lists the pass keys available for options that take a pass key."),
      opt[Unit]("quiet").abbr("q")
        .action((_, c) => c.copy(logLevels = c.logLevels ++ Seq(("vct", Verbosity.Error), ("viper.api", Verbosity.Error))))
        .text("Instruct VerCors to only log errors."),
      opt[Unit]("verbose").abbr("v")
        .action((_, c) => c.copy(logLevels = c.logLevels ++ Seq(("vct", Verbosity.Debug), ("viper.api", Verbosity.Debug))))
        .text("Instruct VerCors to output debug information"),
      opt[Unit]("progress").abbr("p")
        .action((_, c) => c.copy(progress = true))
        .text("Print progress information, even if stdout is not a tty."),

      opt[(String, Verbosity)]("dev-log-verbosity").unbounded().hidden().keyValueName("<loggerKey>", "<verbosity>")
        .action((tup, c) => c.copy(logLevels = c.logLevels :+ tup))
        .text("Set the log level for a custom logger key"),

      note(""),
      note("Verification Mode"),
      opt[Unit]("verify")
        .action((_, c) => c.copy(mode = Mode.Verify))
        .text("Enable verification mode: instruct VerCors to verify the given files (default)"),

      opt[Language]("lang").valueName("{java|c|i|pvl|silver}")
        .action((lang, c) => c.copy(language = Some(lang)))
        .text("Do not detect the language from the file extension, but force a specific language parser for all files"),
      opt[Backend]("backend").valueName("{silicon|carbon}")
        .action((backend, c) => c.copy(backend = backend))
        .text("Set the backend to verify with (default: silicon)"),
      opt[Option[PathOrStd]]("backend-file").valueName("<path>")
        .action((backendFile, c) => c.copy(backendFile = backendFile))
        .text("In addition to verification, output the resulting AST for the backend to a file"),
      opt[Unit]("backend-debug")
        .action((_, c) => c.copy(logLevels = c.logLevels :+ ("viper", Verbosity.Debug)))
        .text("Instruct the backend to print as much debugging information as possible"),

      opt[(String, PathOrStd)]("output-after-pass").unbounded().keyValueName("<pass>", "<path>")
        .action((output, c) => c.copy(outputAfterPass = c.outputAfterPass ++ Map(output)))
        .text("Print the AST after a pass key"),
      opt[(String, PathOrStd)]("output-before-pass").unbounded().keyValueName("<pass>", "<path>")
        .action((output, c) => c.copy(outputBeforePass = c.outputBeforePass ++ Map(output)))
        .text("Print the AST before a pass key"),

      opt[Unit]("skip-backend")
        .action((_, c) => c.copy(skipBackend = true))
        .text("Stop VerCors successfully before the backend is used to verify the program"),
      opt[Unit]("skip-translation")
        .action((_, c) => c.copy(skipTranslation = true))
        .text("Stop VerCors successully immediately after the file is parsed and resolved, and do no further processing"),
      opt[String]("skip-translation-after").valueName("<pass>")
        .action((pass, c) => c.copy(skipTranslationAfter = Some(pass)))
        .text("Stop VerCors successfully after executing the transformation pass with the supplied key"),
      opt[String]("skip-pass").unbounded().valueName("<pass>")
        .action((pass, c) => c.copy(skipPass = c.skipPass + pass))
        .text("Skip the passes that have the supplied keys"),

      opt[(MinimizeName, MinimizeMode)]("minimize" /* TODO (RR): British or american? */).unbounded().keyValueName("<fullyQualifiedName>", "focus|ignore")
        .action((tup, c) => c.copy(minimizeNames = c.minimizeNames + (tup._1 -> tup._2)))
        .text("Ignore entities that are not needed to verify the method/function. Fully qualified name is package.Class.callable for static entities, package.callable for top level entities, or just callable for top-level unpackaged entities"),

      opt[Unit]("dev-abrupt-exc").hidden()
        .action((_, c) => c.copy(devAbruptExc = true))
        .text("Encode all abrupt control flow using exception, even when not necessary"),

      opt[String]("dev-simplify-debug-in").unbounded().hidden().valueName("<declaration>")
        .action((decl, c) => c.copy(devSimplifyDebugIn = c.devSimplifyDebugIn :+ decl))
        .text("Debug simplifications below a declaration preferredName (recommended to inspect --output-before-pass simplify=-)"),
      opt[Unit]("dev-simplify-debug-match").hidden()
        .action((_, c) => c.copy(devSimplifyDebugMatch = true))
        .text("Debug matched expressions in simplifications"),
      opt[Unit]("dev-simplify-debug-match-long").hidden()
        .action((_, c) => c.copy(devSimplifyDebugMatchShort = false))
        .text("Use long form to print matched expressions in sipmlifications"),
      opt[Unit]("dev-simplify-debug-no-match").hidden()
        .action((_, c) => c.copy(devSimplifyDebugNoMatch = true))
        .text("Debug expressions that do not match in simplifications"),
      opt[String]("dev-simplify-debug-filter-input-kind").hidden()
        .action((kind, c) => c.copy(devSimplifyDebugFilterInputKind = Some(kind)))
        .text("Debug only expressions of a certain kind by simple class name"),
      opt[String]("dev-simplify-debug-filter-rule").hidden()
        .action((rule, c) => c.copy(devSimplifyDebugFilterRule = Some(rule)))
        .text("Debug only applications of a particular rule, by name"),

      opt[Map[String, String]]("c-define").valueName("<macro>=<defn>,...")
        .action((defines, c) => c.copy(cDefine = defines))
        .text("Pass -D options to the C preprocessor"),

      opt[Seq[PathOrStd]]("paths-simplify").valueName("<simplify.pvl>,...")
        .action((paths, c) => c.copy(simplifyPaths = paths))
        .text("Specify a chain of files to use that contain axiomatic simplification rules"),
      opt[Seq[PathOrStd]]("paths-simplify-after-relations").valueName("<simplify.pvl>,...")
        .action((paths, c) => c.copy(simplifyPathsAfterRelations = paths))
        .text("Specify a chain of files to use the contain axiomatic simplification rules, which will be applied after quantified integer relations are simplified"),

      opt[Path]("path-adt").valueName("<path>")
        .action((path, c) => c.copy(adtPath = path))
        .text("Use a custom directory that contains definitions for all internal types encoded as axiomatic datatypes (array, option, any, etc.)"),
      opt[Path]("path-cc").valueName("<path>")
        .action((path, c) => c.copy(cc = path))
        .text("Set the C compiler to use for preprocessing"),
      opt[Path]("path-c-system").valueName("<path>")
        .action((path, c) => c.copy(cIncludePath = path))
        .text("Set the include path for system headers (-isystem)"),
      opt[Path]("path-jre").valueName("<path>")
        .action((path, c) => c.copy(jrePath = path))
        .text("Set the directory where specified JRE files are stored"),
      opt[Path]("path-z3").valueName("<path>")
        .action((path, c) => c.copy(z3Path = path))
        .text("Set the location of the z3 binary"),
      opt[Path]("path-boogie").valueName("<path>")
        .action((path, c) => c.copy(boogiePath = path))
        .text("Set the location of the boogie binary"),
      opt[Path]("path-c-preprocessor").valueName("<path>")
        .action((path, c) => c.copy(cPreprocessorPath = path))
        .text("Set the location of the C preprocessor binary"),

      note(""),
      note("VeyMont Mode"),
      opt[Unit]("veymont")
        .action((_, c) => c.copy(mode = Mode.VeyMont))
        .text("Enable VeyMont mode: decompose the global program from the input files into several local programs that can be executed in parallel")
        .children(
          opt[PathOrStd]("veymont-output").required().valueName("<path>")
            .action((path, c) => c.copy(veymontOutput = path))
        ),

      note(""),
      note("Batch Testing Mode"),
      opt[Unit]("test")
        .action((_, c) => c.copy(mode = Mode.BatchTest))
        .text("Enable batch testing mode: execute all tests in a directory")
        .children(
          opt[Path]("test-dir").required().valueName("<path>")
            .action((path, c) => c.copy(testDir = path))
            .text("The directory from which to run all tests"),
          opt[Seq[Backend]]("test-filter-backend").valueName("<backend>,...")
            .action((backends, c) => c.copy(testFilterBackend = Some(backends))),
          opt[Seq[String]]("test-filter-include-suite").valueName("<suite>,...")
            .action((suites, c) => c.copy(testFilterIncludeOnlySuites = Some(suites))),
          opt[Seq[String]]("test-filter-exclude-suite").valueName("<suite>,...")
            .action((suites, c) => c.copy(testFilterExcludeSuites = Some(suites))),
          opt[Int]("test-workers")
            .action((n, c) => c.copy(testWorkers = n))
            .text("Number of threads to start to run tests (default: 1)"),
          opt[Unit]("test-coverage")
            .action((_, c) => c.copy(testCoverage = true))
            .text("Generate a coverage report"),
          opt[Unit]("test-failing-first")
            .action((_, c) => c.copy(testFailingFirst = true))
            .text("When run twice with this option, VerCors will run the tests that failed the previous time first (cancelling a run is safe)"),
          opt[Unit]("test-generate-failing-run-configs")
            .action((_, c) => c.copy(testGenerateFailingRunConfigs = true))
            .text("Generates Intellij IDEA run configurations for tests that fail (and deletes recovered tests, cancelling a run is safe)"),
          opt[Unit]("test-ci-output")
            .action((_, c) => c.copy(testCIOutput = true))
            .text("Tailor the logging output for a CI run")
        ),

      note(""),
      note(""),
      arg[PathOrStd]("<path>...").unbounded().optional()
        .action((path, c) => c.copy(inputs = c.inputs :+ path))
        .text("List of input files to process")
    )
  }

  def parse(args: Array[String]): Option[Options] =
    OParser.parse(parser, args, Options())
}

case class Options
(
  mode: Mode = Mode.Verify,
  inputs: Seq[PathOrStd] = Nil,
  logLevels: Seq[(String, Verbosity)] = Seq(
    ("vct", Verbosity.Info),
    ("viper", Verbosity.Off),
    ("viper.api", Verbosity.Info),
  ),
  progress: Boolean = false,

  // Verify Options
  language: Option[Language] = None,
  backend: Backend = Backend.Silicon,
  backendFile: Option[PathOrStd] = None,

  outputAfterPass: Map[String, PathOrStd] = Map.empty,
  outputBeforePass: Map[String, PathOrStd] = Map.empty,

  skipBackend: Boolean = false,
  skipTranslation: Boolean = false,
  skipTranslationAfter: Option[String] = None,
  skipPass: Set[String] = Set.empty,

  minimizeNames: Map[MinimizeName, MinimizeMode] = Map().empty,

  cDefine: Map[String, String] = Map.empty,

  simplifyPaths: Seq[PathOrStd] = Seq("pushin", "simplify").map(name => PathOrStd.Path(Resources.getSimplificationPath(name))),
  simplifyPathsAfterRelations: Seq[PathOrStd] = Seq("simplify").map(name => PathOrStd.Path(Resources.getSimplificationPath(name))),
  adtPath: Path = Resources.getAdtPath,
  cc: Path = Resources.getCcPath,
  cIncludePath: Path = Resources.getCIncludePath,
  jrePath: Path = Resources.getJrePath,
  z3Path: Path = viper.api.Resources.getZ3Path,
  boogiePath: Path = viper.api.Resources.getBoogiePath,
  cPreprocessorPath: Path = Resources.getCcPath,

  // Verify options - hidden
  devAbruptExc: Boolean = false,
  devSimplifyDebugIn: Seq[String] = Nil,
  devSimplifyDebugMatch: Boolean = false,
  devSimplifyDebugMatchShort: Boolean = true,
  devSimplifyDebugNoMatch: Boolean = false,
  devSimplifyDebugFilterInputKind: Option[String] = None,
  devSimplifyDebugFilterRule: Option[String] = None,

  // VeyMont options
  veymontOutput: PathOrStd = null, // required

  // Batch test options
  testDir: Path = null, // required
  testFilterBackend: Option[Seq[Backend]] = None,
  testFilterIncludeOnlySuites: Option[Seq[String]] = None,
  testFilterExcludeSuites: Option[Seq[String]] = None,
  testWorkers: Int = 1,
  testCoverage: Boolean = false,
  testFailingFirst: Boolean = false,
  testGenerateFailingRunConfigs: Boolean = false,
  testCIOutput: Boolean = false,
)