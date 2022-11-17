package vct.main.stages

import hre.stages.Stage
import org.antlr.v4.runtime.CharStreams
import vct.col.ast.{AddrOf, CGlobalDeclaration, Expr, Program, Refute, VerificationContext}
import vct.col.check.CheckError
import vct.col.rewrite.lang.{LangSpecificToCol, LangSpecificToColArgs, LangTypesToCol}
import vct.col.origin.{ExpectedError, FileSpanningOrigin, Origin}
import vct.col.resolve.lang.{C, Java}
import vct.col.resolve.{Resolve, ResolveReferences, ResolveTypes}
import vct.col.rewrite.Generation
import vct.col.rewrite.bip.IsolateBipGlue
import vct.importer.JavaLibraryLoader
import vct.main.Main.TemporarilyUnsupported
import vct.main.stages.Resolution.InputResolutionError
import vct.main.stages.Transformation.TransformationCheckError
import vct.options.Options
import vct.parsers.{ColJavaParser, FileNotFound, ParseResult}
import vct.parsers.transform.{BlameProvider, ReadableOriginProvider, RedirectOriginProvider}
import vct.resources.Resources
import vct.result.VerificationError.UserError

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
      javaLibraryPath = options.jrePath,
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
    // TODO (RR): The behavior of redirecting origins works now but its is ugly, refactor
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

case class Resolution[G <: Generation]
(
  blameProvider: BlameProvider,
  javaLibraryPath: Path = Resources.getJrePath,
  bipSynchrons: Seq[((String, String), (String, String))] = Seq(),
  bipDatas: Seq[((String, String), (String, String))] = Seq(),
) extends Stage[ParseResult[G], VerificationContext[_ <: Generation]] {
  override def friendlyName: String = "Name Resolution"
  override def progressWeight: Int = 1

  override def run(in: ParseResult[G]): VerificationContext[_ <: Generation] = {
    in.decls.foreach(_.transSubnodes.foreach {
      case decl: CGlobalDeclaration[_] => decl.decl.inits.foreach(init => {
        if(C.getDeclaratorInfo(init.decl).params.isEmpty) {
          throw TemporarilyUnsupported("GlobalCVariable", Seq(decl))
        }
      })
      case addrOf: AddrOf[_] => throw TemporarilyUnsupported("&", Seq(addrOf))
      case _ =>
    })

    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(in.decls)(blameProvider())
    val isolatedBipProgram = IsolateBipGlue.isolate(parsedProgram)
    val extraDecls = ResolveTypes.resolve(isolatedBipProgram, Some(JavaLibraryLoader(javaLibraryPath, blameProvider)))
    val joinedProgram = Program(isolatedBipProgram.declarations ++ extraDecls)(blameProvider())
    val typedProgram = LangTypesToCol().dispatch(joinedProgram)
    ResolveReferences.resolve(typedProgram, MyLocalJavaParser(blameProvider)) match {
      case Nil => // ok
      case some => throw InputResolutionError(some)
    }
    val resolvedProgram = LangSpecificToCol.withArg(LangSpecificToColArgs(bipSynchrons, bipDatas))().dispatch(typedProgram)
    resolvedProgram.check match {
      case Nil => // ok
      // PB: This explicitly allows LangSpecificToCol to generate invalid ASTs, and will blame the input for them. The
      // alternative is that we duplicate a lot of checks (e.g. properties of Local hold for PVLLocal, JavaLocal, etc.)
      case some => throw InputResolutionError(some)
    }

    VerificationContext(resolvedProgram, in.expectedErrors)
  }
}
