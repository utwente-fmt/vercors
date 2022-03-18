package vct.main.stages

import vct.col.ast.Program
import vct.col.check.CheckError
import vct.col.newrewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.col.origin.{FileSpanningOrigin, Origin}
import vct.col.resolve.{Java, ResolveReferences, ResolveTypes}
import vct.col.rewrite.Generation
import vct.col.util.ExpectedError
import vct.java.JavaLibraryLoader
import vct.main.stages.Resolution.InputResolutionError
import vct.main.stages.Transformation.TransformationCheckError
import vct.options.Options
import vct.parsers.ParseResult
import vct.parsers.transform.BlameProvider
import vct.resources.Resources
import vct.result.VerificationError.UserError

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

case class Resolution[G <: Generation]
(
  blameProvider: BlameProvider,
  withJava: Boolean = true,
  javaLibraryPath: Path = Resources.getJrePath,
) extends Stage[ParseResult[G], (Program[_ <: Generation], Seq[ExpectedError])] {
  override def friendlyName: String = "Name Resolution"
  override def progressWeight: Int = 1

  override def run(in: ParseResult[G]): (Program[_ <: Generation], Seq[ExpectedError]) = {
    implicit val o: Origin = FileSpanningOrigin

    val parsedProgram = Program(in.decls, if(withJava) Some(Java.JAVA_LANG_OBJECT[G]) else None)(blameProvider())
    val extraDecls = ResolveTypes.resolve(parsedProgram, if(withJava) Some(JavaLibraryLoader(javaLibraryPath, blameProvider)) else None)
    val joinedProgram = Program(parsedProgram.declarations ++ extraDecls, parsedProgram.rootClass)(blameProvider())
    val typedProgram = LangTypesToCol().dispatch(joinedProgram)
    ResolveReferences.resolve(typedProgram) match {
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
