package vct.main.stages

import org.antlr.v4.runtime.CharStreams
import vct.col.ast.{AddrOf, CGlobalDeclaration, Expr, Program, Refute}
import vct.col.check.CheckError
import vct.col.newrewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.col.origin.{FileSpanningOrigin, Origin}
import vct.col.resolve.{C, Java, Resolve, ResolveReferences, ResolveTypes}
import vct.col.rewrite.Generation
import vct.col.util.ExpectedError
import vct.java.JavaLibraryLoader
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
      withJava = true,
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
    val cjp = ColJavaParser(RedirectOriginProvider(o, ReadableOriginProvider(sr)), blameProvider)
    val x = try {
        sr.read { reader =>
          cjp.parseExpr[G](CharStreams.fromReader(reader, sr.fileName))
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
  withJava: Boolean = true,
  javaLibraryPath: Path = Resources.getJrePath,
) extends Stage[ParseResult[G], (Program[_ <: Generation], Seq[ExpectedError])] {
  override def friendlyName: String = "Name Resolution"
  override def progressWeight: Int = 1

  override def run(in: ParseResult[G]): (Program[_ <: Generation], Seq[ExpectedError]) = {
    in.decls.foreach(_.transSubnodes.foreach {
      case decl: CGlobalDeclaration[_] => decl.decl.inits.foreach(init => {
        if(C.getDeclaratorInfo(init.decl).params.isEmpty) {
          throw TemporarilyUnsupported("GlobalCVariable", Seq(decl))
        }
      })
      case addrOf: AddrOf[_] => throw TemporarilyUnsupported("&", Seq(addrOf))
      case ref: Refute[_] => throw TemporarilyUnsupported("Refute", Seq(ref))
      case _ =>
    })

    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(in.decls, if(withJava) Some(Java.JAVA_LANG_OBJECT[G]) else None)(blameProvider())
    val extraDecls = ResolveTypes.resolve(parsedProgram, if(withJava) Some(JavaLibraryLoader(javaLibraryPath, blameProvider)) else None)
    val joinedProgram = Program(parsedProgram.declarations ++ extraDecls, parsedProgram.rootClass)(blameProvider())
    val typedProgram = LangTypesToCol().dispatch(joinedProgram)
    ResolveReferences.resolve(typedProgram, MyLocalJavaParser(blameProvider)) match {
      case Nil => // ok
      case some => throw InputResolutionError(some)
    }
    val resolvedProgram = LangSpecificToCol().dispatch(typedProgram)
    resolvedProgram.check match {
      case Nil => // ok
      case some => throw TransformationCheckError(some)
    }

    (resolvedProgram, in.expectedError)
  }
}
