package vct.importer

import hre.io.Readable
import vct.col.ast.Program
import vct.col.origin.{Blame, VerificationFailure}
import vct.col.rewrite.Disambiguate
import vct.main.stages.Resolution
import vct.parsers.ColPVLParser
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
    val context = Resolution(ConstantBlameProvider(LibraryFileBlame), withJava = false).run(res)
    assert(context.expectedErrors.isEmpty)
    val unambiguousProgram: Program[_] = Disambiguate().dispatch(context.program)
    unambiguousProgram.asInstanceOf[Program[G]]
  }
}
