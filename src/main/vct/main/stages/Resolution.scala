package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.stages.Stage
import vct.col.ast.{AddrOf, ApplicableContract, CGlobalDeclaration, Expr, GlobalDeclaration, LlvmFunctionContract, LlvmGlobal, Program, Refute, Verification, VerificationContext}
import org.antlr.v4.runtime.CharStreams
import vct.col.check.CheckError
import vct.col.rewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.col.origin.{ExpectedError, FileSpanningOrigin, LLVMOrigin, Origin}
import vct.col.resolve.lang.{C, Java}
import vct.col.resolve.{Resolve, ResolveReferences, ResolveTypes}
import vct.col.rewrite.Generation
import vct.col.rewrite.bip.IsolateBipGlue
import vct.importer.JavaLibraryLoader
import vct.main.Main.TemporarilyUnsupported
import vct.main.stages.Resolution.InputResolutionError
import vct.main.stages.Transformation.TransformationCheckError
import vct.options.Options
import vct.options.types.ClassPathEntry
import vct.parsers.ParseResult
import vct.parsers.transform.BlameProvider
import vct.parsers.{ColJavaParser, ColLLVMParser, FileNotFound, ParseResult}
import vct.parsers.transform.{BlameProvider, ReadableOriginProvider, RedirectOriginProvider}
import vct.resources.Resources
import vct.result.VerificationError.UserError
import viper.silver.frontend.DefaultStates.Initial

import java.io.{FileNotFoundException, Reader}
import java.nio.file.Path

case object Resolution {
  case class InputResolutionError(errors: Seq[CheckError]) extends UserError {
    override def code: String = "resolutionError"

    override def text: String = errors.map(_.toString).mkString("\n")
  }

  def ofOptions[G <: Generation](options: Options, blameProvider: BlameProvider): Resolution[G] =
    Resolution(
      blameProvider = blameProvider,
      classPath = options.classPath.map {
        case ClassPathEntry.DefaultJre => ResolveTypes.JavaClassPathEntry.Path(Resources.getJrePath)
        case ClassPathEntry.SourcePackageRoot => ResolveTypes.JavaClassPathEntry.SourcePackageRoot
        case ClassPathEntry.SourcePath(root) => ResolveTypes.JavaClassPathEntry.Path(root)
      },
    )
}

case class StringReadable(data: String, fileName: String = "<unknown>") extends hre.io.Readable {
  override def isRereadable: Boolean = true

  override protected def getReader: Reader = new java.io.StringReader(data)
}

case class SpecExprParseError(msg: String) extends UserError {
  override def code: String = "specExprParseError"

  override def text: String = msg
}

case class MyLocalJavaParser(blameProvider: BlameProvider) extends Resolve.SpecExprParser {
  override def parse[G](input: String, o: Origin): Expr[G] = {
    val sr = StringReadable(input)
    val cjp = ColJavaParser(RedirectOriginProvider(o, input), blameProvider)
    val x = try {
      sr.read { reader =>
        cjp.parseExpr[G](CharStreams.fromReader(reader, sr.fileName), false)
      }
    } catch {
      case _: FileNotFoundException => throw FileNotFound(sr.fileName)
    }
    if (x._2.nonEmpty) {
      throw SpecExprParseError("...")
    }
    x._1
  }
}

case class MyLocalLLVMSpecParser(blameProvider: BlameProvider) extends Resolve.SpecContractParser {
  override def parse[G](input: LlvmFunctionContract[G], o: Origin): ApplicableContract[G] = {
    val originProvider = ReadableOriginProvider(input.o match {
      case o: LLVMOrigin => StringReadable(input.value, o.fileName)
      case _ => StringReadable(input.value)
    })
    val charStream = CharStreams.fromString(input.value)
    ColLLVMParser(originProvider, blameProvider)
      .parseFunctionContract[G](charStream)._1
  }

  override def parse[G](input: LlvmGlobal[G], o: Origin): GlobalDeclaration[G] = {
    val originProvider = ReadableOriginProvider(input.o match {
      case o: LLVMOrigin => StringReadable(input.value, o.fileName)
      case _ => StringReadable(input.value)
    })
    val charStream = CharStreams.fromString(input.value)
    ColLLVMParser(originProvider, blameProvider)
      .parseGlobal(charStream)._1
  }
}

case class Resolution[G <: Generation]
(
  blameProvider: BlameProvider,
  classPath: Seq[ResolveTypes.JavaClassPathEntry] = Seq(
    ResolveTypes.JavaClassPathEntry.Path(Resources.getJrePath),
    ResolveTypes.JavaClassPathEntry.SourcePackageRoot
  ),
) extends Stage[ParseResult[G], Verification[_ <: Generation]] with LazyLogging {
  override def friendlyName: String = "Name Resolution"

  override def progressWeight: Int = 1

  override def run(in: ParseResult[G]): Verification[_ <: Generation] = {
    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(in.decls)(blameProvider())
    val isolatedBipProgram = IsolateBipGlue.isolate(parsedProgram)
    val extraDecls = ResolveTypes.resolve(isolatedBipProgram, Some(JavaLibraryLoader(blameProvider)), classPath)
    val joinedProgram = Program(isolatedBipProgram.declarations ++ extraDecls)(blameProvider())
    val typedProgram = LangTypesToCol().dispatch(joinedProgram)
    ResolveReferences.resolve(typedProgram, MyLocalJavaParser(blameProvider), MyLocalLLVMSpecParser(blameProvider)) match {
      case Nil => // ok
      case some => throw InputResolutionError(some)
    }
    val resolvedProgram = LangSpecificToCol().dispatch(typedProgram)
    resolvedProgram.check match {
      case Nil => // ok
      // PB: This explicitly allows LangSpecificToCol to generate invalid ASTs, and will blame the input for them. The
      // alternative is that we duplicate a lot of checks (e.g. properties of Local hold for PVLLocal, JavaLocal, etc.)
      case some => throw InputResolutionError(some)
    }

    Verification(Seq(VerificationContext(resolvedProgram)), in.expectedErrors)
  }
}
