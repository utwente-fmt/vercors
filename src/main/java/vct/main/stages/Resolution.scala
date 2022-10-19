package vct.main.stages

import hre.stages.Stage
import vct.col.ast.{AddrOf, CGlobalDeclaration, Program, Refute, VerificationContext}
import vct.col.check.CheckError
import vct.col.rewrite.lang.{LangSpecificToCol, LangTypesToCol}
import vct.col.origin.{ExpectedError, FileSpanningOrigin, Origin}
import vct.col.resolve.lang.{C, Java}
import vct.col.resolve.{ResolveReferences, ResolveTypes}
import vct.col.rewrite.Generation
import vct.importer.JavaLibraryLoader
import vct.main.Main.TemporarilyUnsupported
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
      javaLibraryPath = options.jrePath,
    )
}

case class Resolution[G <: Generation]
(
  blameProvider: BlameProvider,
  javaLibraryPath: Path = Resources.getJrePath,
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
    val extraDecls = ResolveTypes.resolve(parsedProgram, Some(JavaLibraryLoader(javaLibraryPath, blameProvider)))
    val joinedProgram = Program(parsedProgram.declarations ++ extraDecls)(blameProvider())
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

    VerificationContext(resolvedProgram, in.expectedErrors)
  }
}
