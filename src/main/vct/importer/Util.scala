package vct.importer

import hre.io.{RWFile, Readable}
import vct.cache.Caches
import vct.col.ast.{Deserialize, JavaClass, JavaNamespace, Program, Serialize}
import vct.col.origin.{Blame, Origin, ReadableOrigin, VerificationFailure}
import vct.col.rewrite.Disambiguate
import vct.main.stages.Resolution
import vct.parsers.{ColJavaParser, ColPVLParser}
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError.UserError

import java.nio.file.Files
import scala.util.Using

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
    val text = readable.readToCompletion()
    val cacheDir = Caches.getLibraryCache.resolve("%02x" format text.hashCode())
    val pinnedLibrary = cacheDir.resolve("library.in")
    val result = cacheDir.resolve("library.colpb")

    if(!Files.exists(cacheDir) || RWFile(pinnedLibrary).readToCompletion() != text) {
      val res = ColPVLParser(Origin(Seq(ReadableOrigin(readable))), ConstantBlameProvider(LibraryFileBlame)).parse(readable)
      val context = Resolution(ConstantBlameProvider(LibraryFileBlame)).run(res)
      val unambiguousProgram: Program[_] = Disambiguate().dispatch(context.tasks.head.program)

      Files.createDirectories(cacheDir)
      Using(Files.newOutputStream(pinnedLibrary)) { out =>
        out.write(text.getBytes)
      }

      Using(Files.newOutputStream(result)) { out =>
        Serialize.serialize(unambiguousProgram).writeTo(out)
      }
    }

    Using(Files.newInputStream(result)) { in =>
      Deserialize.deserializeProgram[G](vct.col.ast.serialize.Program.parseFrom(in), 0)
    }.get
  }

  case class JavaLoadError(error: String) extends UserError {
    override def code: String = "JavaClassLoadError"

    override def text: String = error
  }

  def loadJavaClass[G](readable: Readable): JavaClass[G] =
    ColJavaParser(Origin(Seq(ReadableOrigin(readable))), ConstantBlameProvider(LibraryFileBlame)).parse(readable).decls match {
      case Seq(javaNamespace: JavaNamespace[G @unchecked]) => javaNamespace.declarations match {
        case Seq(javaClass: JavaClass[G]) => javaClass
        case seq => throw JavaLoadError("Expected to load exactly one Java class but found " + seq.size)
      }
      case seq => throw JavaLoadError("Expected to load exactly one Java name space but found " + seq.size)
    }
}
