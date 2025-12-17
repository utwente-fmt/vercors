package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.io.LiteralReadable
import hre.stages.Stage
import vct.col.ast.{
  ApplicableContract,
  Expr,
  Function,
  GlobalDeclaration,
  VCLLVMFunctionContract,
  LLVMGlobalSpecification,
  Program,
  Verification,
  VerificationContext,
}
import vct.col.check.CheckError
import vct.col.origin.{FileSpanningOrigin, Origin, ReadableOrigin}
import vct.col.resolve.{Resolve, ResolveReferences, ResolveTypes}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.rewrite.bip.IsolateBipGlue
import vct.col.typerules.{PlatformContext, TypeSize}
import vct.rewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.importer.JavaLibraryLoader
import vct.main.stages.Resolution.{InputResolutionError, TargetError}
import vct.options.Options
import vct.options.types.{ClassPathEntry, PathOrStd}
import vct.parsers.debug.DebugOptions
import vct.parsers.parser.{ColJavaParser, ColLLVMContractParser, ColPVLParser}
import vct.parsers.transform.BlameProvider
import vct.parsers.ParseResult
import vct.resources.Resources
import vct.result.VerificationError.UserError

import java.io.{
  InputStreamReader,
  OutputStreamWriter,
  StringReader,
  StringWriter,
}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

case object Resolution {
  case class InputResolutionError(errors: Seq[CheckError]) extends UserError {
    override def code: String =
      s"resolutionError:${errors.map(_.subcode).mkString(",")}"
    override def text: String = errors.map(_.message(_.o)).mkString("\n")
  }

  case class TargetError(target: String, output: String) extends UserError {
    override def code: String = "targetError"
    override def text: String =
      s"Failed to get information for target: '$target', response from clang was:\n$output"
  }

  def ofOptions[G <: Generation](
      options: Options,
      blameProvider: BlameProvider,
  ): Resolution[G] =
    Resolution(
      blameProvider = blameProvider,
      parserDebugOptions = options.getParserDebugOptions,
      classPath = options.classPath.map {
        case ClassPathEntry.DefaultJre =>
          ResolveTypes.JavaClassPathEntry.Path(Resources.getJrePath)
        case ClassPathEntry.SourcePackageRoot =>
          ResolveTypes.JavaClassPathEntry.SourcePackageRoot
        case ClassPathEntry.SourcePath(root) =>
          ResolveTypes.JavaClassPathEntry.Path(root)
      },
      if (options.contractImportFile.isDefined) {
        val res = ColPVLParser(options.getParserDebugOptions, blameProvider)
          .parse[G](
            options.contractImportFile.get,
            Origin(Seq(ReadableOrigin(options.contractImportFile.get))),
          )
        res.decls
      } else { Seq() },
      options.generatePermissions,
      options.cc,
      options.cIncludePath,
      options.targetString,
    )
}

case class SpecExprParseError(msg: String) extends UserError {
  override def code: String = "specExprParseError"

  override def text: String = msg
}

case class MyLocalJavaParser(
    blameProvider: BlameProvider,
    debugOptions: DebugOptions,
) extends Resolve.SpecExprParser {
  override def parse[G](input: String, o: Origin): Expr[G] = {
    val sr = LiteralReadable("<string data>", input)
    val cjp = ColJavaParser(debugOptions, blameProvider)
    val x = cjp.parseExpr[G](sr)
    if (x._2.nonEmpty) { throw SpecExprParseError("...") }
    x._1
  }
}

case class MyLocalLLVMSpecParser(
    blameProvider: BlameProvider,
    debugOptions: DebugOptions,
) extends Resolve.SpecContractParser {
  override def parse[G](
      input: VCLLVMFunctionContract[G],
      o: Origin,
  ): ApplicableContract[G] =
    ColLLVMContractParser(debugOptions, blameProvider)
      .parseFunctionContract[G](new StringReader(input.value), o)._1

  override def parse[G](
      input: LLVMGlobalSpecification[G],
      o: Origin,
  ): Seq[GlobalDeclaration[G]] =
    ColLLVMContractParser(debugOptions, blameProvider)
      .parseReader[G](new StringReader(input.value), o).decls
}

case class Resolution[G <: Generation](
    blameProvider: BlameProvider,
    parserDebugOptions: DebugOptions,
    classPath: Seq[ResolveTypes.JavaClassPathEntry] = Seq(
      ResolveTypes.JavaClassPathEntry.Path(Resources.getJrePath),
      ResolveTypes.JavaClassPathEntry.SourcePackageRoot,
    ),
    importedDeclarations: Seq[GlobalDeclaration[G]] = Seq(),
    generatePermissions: Boolean = false,
    cc: Path = Resources.getCcPath,
    cSystemInclude: Path = Resources.getCIncludePath,
    targetString: Option[String] = None,
) extends Stage[ParseResult[G], Verification[_ <: Generation]]
    with LazyLogging {
  override def friendlyName: String = "Name Resolution"

  override def progressWeight: Int = 1

  override def run(in: ParseResult[G]): Verification[_ <: Generation] = {
    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(in.decls)(blameProvider())
    val isolatedBipProgram = IsolateBipGlue.isolate(parsedProgram)
    val extraDecls = ResolveTypes.resolve(
      isolatedBipProgram,
      Some(JavaLibraryLoader(blameProvider, parserDebugOptions)),
      classPath,
    )
    val joinedProgram =
      Program(isolatedBipProgram.declarations ++ extraDecls)(blameProvider())

    val platformContext = queryPlatformContext()
    logger.debug(s"Determined platform context: $platformContext")

    val typedProgram = LangTypesToCol(platformContext).dispatch(joinedProgram)
    val javaParser = MyLocalJavaParser(blameProvider, parserDebugOptions)
    val llvmParser = MyLocalLLVMSpecParser(blameProvider, parserDebugOptions)
    val typedImports =
      if (importedDeclarations.isEmpty) { Seq() }
      else {
        val ast = LangTypesToCol(platformContext)
          .dispatch(Program(importedDeclarations)(blameProvider()))
        ResolveReferences.resolve(ast, javaParser, llvmParser, Seq())
        LangSpecificToCol(generatePermissions).dispatch(ast)
          .asInstanceOf[Program[Rewritten[G]]].declarations
      }
    ResolveReferences
      .resolve(typedProgram, javaParser, llvmParser, typedImports) match {
      case Nil => // ok
      case some => throw InputResolutionError(some)
    }
    // (TODO (AS): I know this is ugly, it'll be removed after the Pallas contracts are working
    val mergedProgram =
      Program[Rewritten[G]](typedProgram.declarations ++ typedImports.collect {
        case f: Function[Rewritten[G]] => f
      })(typedProgram.blame)(typedProgram.o)
    val resolvedProgram = LangSpecificToCol(generatePermissions)
      .dispatch(mergedProgram)
    resolvedProgram.check match {
      case Nil => // ok
      // PB: This explicitly allows LangSpecificToCol to generate invalid ASTs, and will blame the input for them. The
      // alternative is that we duplicate a lot of checks (e.g. properties of Local hold for PVLLocal, JavaLocal, etc.)
      case some => throw InputResolutionError(some)
    }

    Verification(Seq(VerificationContext(resolvedProgram)), in.expectedErrors)
  }

  private def queryPlatformContext(): PlatformContext = {
    if (targetString.isEmpty)
      return PlatformContext.DEFAULT
    val target = targetString.get
    val process =
      new ProcessBuilder(
        cc.toString,
        "-C",
        "-E",
        "-nostdinc",
        "-nocudainc",
        "-nocudalib",
        "--cuda-host-only",
        "-target",
        target,
        "-",
      ).start()
    val queryFile = PathOrStd.Path(cSystemInclude.resolve("platform_query.c"))
    new Thread(
      () => {
        val writer =
          new OutputStreamWriter(
            process.getOutputStream,
            StandardCharsets.UTF_8,
          )
        try {
          val written = queryFile.read(_.transferTo(writer))
          logger.debug(s"Wrote $written bytes to clang")
        } finally { writer.close() }
      },
      "[VerCors] clang stdout writer",
    ).start()
    process.waitFor()

    val writer = new StringWriter()
    new InputStreamReader(process.getInputStream).transferTo(writer)

    if (process.exitValue() != 0) {
      new InputStreamReader(process.getErrorStream).transferTo(writer)
      writer.close()
      throw TargetError(target, writer.toString)
    }

    val map =
      writer.toString.linesIterator.map(_.split('=')).collect {
        case split if split.length == 2 =>
          (
            split(0),
            try { BigInt(split(1)) }
            catch {
              case _: NumberFormatException =>
                throw TargetError(target, writer.toString)
            },
          )
      }.toMap

    PlatformContext(
      charSize = TypeSize.Exact(
        map.getOrElse("char", throw TargetError(target, writer.toString))
      ),
      shortSize = TypeSize.Exact(
        map.getOrElse("short", throw TargetError(target, writer.toString))
      ),
      intSize = TypeSize.Exact(
        map.getOrElse("int", throw TargetError(target, writer.toString))
      ),
      longSize = TypeSize.Exact(
        map.getOrElse("long", throw TargetError(target, writer.toString))
      ),
      longLongSize = TypeSize.Exact(
        map.getOrElse("long long", throw TargetError(target, writer.toString))
      ),
      pointerSize = TypeSize.Exact(
        map.getOrElse("intptr_t", throw TargetError(target, writer.toString))
      ),
    )
  }
}
