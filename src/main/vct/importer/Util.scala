package vct.importer

import hre.io.Readable
import vct.col.ast.{JavaClass, JavaNamespace, Program}
import vct.col.origin.{Blame, VerificationFailure}
import vct.col.rewrite.Disambiguate
import vct.main.stages.Resolution
import vct.parsers.{ColJavaParser, ColPVLParser}
import vct.parsers.transform.{ConstantBlameProvider, ReadableOriginProvider}
import vct.result.VerificationError.UserError

case object Util {
  case object LibraryFileBlame extends Blame[VerificationFailure] {
    override def blame(error: VerificationFailure): Unit =
      throw LibraryFileError(error)
  }

  case class LibraryFileError(error: VerificationFailure) extends UserError {
    override def code: String = "lib"
    override def text: String =
      "A verification condition failed inside a file loaded as a library file, which is never supposed to happen. The internal error is:\n" + error.toString
  }

  def loadPVLLibraryFile[G](readable: Readable): Program[G] = {
    val res = ColPVLParser(ReadableOriginProvider(readable), ConstantBlameProvider(LibraryFileBlame)).parse(readable)
    val context = Resolution(ConstantBlameProvider(LibraryFileBlame)).run(res)
    val unambiguousProgram: Program[_] = Disambiguate().dispatch(context.tasks.head.program)
    unambiguousProgram.asInstanceOf[Program[G]]
  }

  case class JavaLoadError(error: String) extends UserError {
    override def code: String = "JavaClassLoadError"

    override def text: String = error
  }

  def loadJavaClass[G](readable: Readable): JavaClass[G] =
    ColJavaParser(ReadableOriginProvider(readable), ConstantBlameProvider(LibraryFileBlame)).parse(readable).decls match {
      case Seq(javaNamespace: JavaNamespace[G @unchecked]) => javaNamespace.declarations match {
        case Seq(javaClass: JavaClass[G]) => javaClass
        case seq => throw JavaLoadError("Expected to load exactly one Java class but found " + seq.size)
      }
      case seq => throw JavaLoadError("Expected to load exactly one Java name space but found " + seq.size)
    }
}
