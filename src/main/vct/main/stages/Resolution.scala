package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.io.LiteralReadable
import hre.stages.Stage
import vct.col.ast.{AddrOf, ApplicableContract, CGlobalDeclaration, Expr, GlobalDeclaration, LlvmFunctionContract, LlvmGlobal, Program, Refute, Verification, VerificationContext}
import org.antlr.v4.runtime.CharStreams
import vct.col.ast._
import vct.col.check.CheckError
import vct.col.origin.{FileSpanningOrigin, InlineBipContext, Origin, OriginFilename, ReadableOrigin}
import vct.col.resolve.{Resolve, ResolveReferences, ResolveTypes}
import vct.col.rewrite.Generation
import vct.col.rewrite.bip.IsolateBipGlue
import vct.rewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.importer.JavaLibraryLoader
import vct.main.stages.Resolution.InputResolutionError
import vct.options.Options
import vct.options.types.ClassPathEntry
import vct.parsers.debug.DebugOptions
import vct.parsers.err.FileNotFound
import vct.parsers.parser.{ColJavaParser, ColLLVMParser}
import vct.parsers.transform.BlameProvider
import vct.parsers.{ParseResult, parser}
import vct.resources.Resources
import vct.result.VerificationError.UserError

import java.io.{FileNotFoundException, Reader, StringReader}
import java.nio.file.NoSuchFileException

case object Resolution {
  case class InputResolutionError(errors: Seq[CheckError]) extends UserError {
    override def code: String = s"resolutionError:${errors.map(_.subcode).mkString(",")}"
    override def text: String = errors.map(_.message(_.o)).mkString("\n")
  }

  def ofOptions[G <: Generation](options: Options, blameProvider: BlameProvider): Resolution[G] =
    Resolution(
      blameProvider = blameProvider,
      parserDebugOptions = options.getParserDebugOptions,
      classPath = options.classPath.map {
        case ClassPathEntry.DefaultJre => ResolveTypes.JavaClassPathEntry.Path(Resources.getJrePath)
        case ClassPathEntry.SourcePackageRoot => ResolveTypes.JavaClassPathEntry.SourcePackageRoot
        case ClassPathEntry.SourcePath(root) => ResolveTypes.JavaClassPathEntry.Path(root)
      },
      options.veymontGeneratePermissions,
      options.devVeymontAllowAssign,
    )
}

case class SpecExprParseError(msg: String) extends UserError {
  override def code: String = "specExprParseError"

  override def text: String = msg
}

case class MyLocalJavaParser(blameProvider: BlameProvider, debugOptions: DebugOptions) extends Resolve.SpecExprParser {
  override def parse[G](input: String, o: Origin): Expr[G] = {
    val sr = LiteralReadable("<string data>", input)
    val cjp = parser.ColJavaParser(debugOptions, blameProvider)
    val x = cjp.parseExpr[G](sr)
    if (x._2.nonEmpty) {
      throw SpecExprParseError("...")
    }
    x._1
  }
}

case class MyLocalLLVMSpecParser(blameProvider: BlameProvider, debugOptions: DebugOptions) extends Resolve.SpecContractParser {
  override def parse[G](input: LlvmFunctionContract[G], o: Origin): ApplicableContract[G] =
    parser.ColLLVMContractParser(debugOptions, blameProvider)
      .parseFunctionContract[G](new StringReader(input.value), o)._1

  override def parse[G](input: LlvmGlobal[G], o: Origin): Seq[GlobalDeclaration[G]] =
    parser.ColLLVMContractParser(debugOptions, blameProvider)
      .parseReader[G](new StringReader(input.value), o).decls
}

case class Resolution[G <: Generation]
(
  blameProvider: BlameProvider,
  parserDebugOptions: DebugOptions,
  classPath: Seq[ResolveTypes.JavaClassPathEntry] = Seq(
    ResolveTypes.JavaClassPathEntry.Path(Resources.getJrePath),
    ResolveTypes.JavaClassPathEntry.SourcePackageRoot
  ),
  veymontGeneratePermissions: Boolean = false,
  veymontAllowAssign: Boolean = false,
) extends Stage[ParseResult[G], Verification[_ <: Generation]] with LazyLogging {
  override def friendlyName: String = "Name Resolution"

  override def progressWeight: Int = 1

  override def run(in: ParseResult[G]): Verification[_ <: Generation] = {
    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(in.decls)(blameProvider())
    val isolatedBipProgram = IsolateBipGlue.isolate(parsedProgram)
    val extraDecls = ResolveTypes.resolve(isolatedBipProgram, Some(JavaLibraryLoader(blameProvider, parserDebugOptions)), classPath)
    val joinedProgram = Program(isolatedBipProgram.declarations ++ extraDecls)(blameProvider())
    val typedProgram = LangTypesToCol().dispatch(joinedProgram)
    ResolveReferences.resolve(typedProgram, MyLocalJavaParser(blameProvider, parserDebugOptions), MyLocalLLVMSpecParser(blameProvider, parserDebugOptions)) match {
      case Nil => // ok
      case some =>
        throw InputResolutionError(some)
    }
    val resolvedProgram = LangSpecificToCol(veymontGeneratePermissions, veymontAllowAssign).dispatch(typedProgram)
    resolvedProgram.check match {
      case Nil => // ok
      // PB: This explicitly allows LangSpecificToCol to generate invalid ASTs, and will blame the input for them. The
      // alternative is that we duplicate a lot of checks (e.g. properties of Local hold for PVLLocal, JavaLocal, etc.)
      case some => throw InputResolutionError(some)
    }

    Verification(Seq(VerificationContext(resolvedProgram)), in.expectedErrors)
  }
}
