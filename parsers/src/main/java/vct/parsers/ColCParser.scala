package vct.parsers
import hre.config.Configuration
import hre.util.FileHelper
import org.antlr.v4.runtime.CharStream
import vct.col.ast.GlobalDeclaration
import vct.parsers.CParser.PreprocessorError
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationResult.{Unreachable, UserError}

import java.io.{File, InputStream}
import java.nio.file.Path
import java.util.Scanner
import scala.jdk.CollectionConverters._

case object CParser {
  case class PreprocessorError(path: Path, errorCode: Int, error: String) extends UserError {
    override def code: String = "preprocessorError"
    override def text: String =
      s"Preprocesing file $path failed with exit code $errorCode:\n$error"
  }
}

case class ColCParser(systemInclude: Path, otherIncludes: Seq[Path], defines: Map[String, String]) extends Parser {
  def interpret(localInclude: Seq[Path], input: String, output: String): Process = {
    // TODO PB: this is not great, should really parse it or so.
    var command = Configuration.currentConfiguration.cpp_command.get().split(' ').toSeq

    command ++= Seq("-nostdinc", "-nocudainc", "-nocudalib", "--cuda-host-only")
    command ++= Seq("-isystem", systemInclude.toAbsolutePath.toString)

    command ++= localInclude.map("-I" + _.toAbsolutePath)
    command ++= otherIncludes.map("-I" + _.toAbsolutePath.toString)
    command ++= defines.map { case (k, v) => s"-D$k=$v" }
    command ++= Seq("-o", output)
    command :+= input

    new ProcessBuilder(command:_*).start()
  }

  override def parse[G](stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] = {
    throw Unreachable("Should not parse C files from an ANTLR CharStream: they need to be interpreted first!")
  }

  override def parse[G](stream: InputStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] = {
    val process = interpret(localInclude=Nil, input="-", output="-")
    new Thread(() => stream.transferTo(process.getOutputStream)).start()
    val result = ColIParser().parse[G](process.getInputStream, originProvider, blameProvider)
    process.destroy()
    result
  }

  override def parse[G](f: File)(originProvider: OriginProvider = null,
                              blameProvider: BlameProvider = null): ParseResult[G] = {
    // TODO PB: this needs some more thinking: we're ignoring the providers here. Really this whole abstraction doesn't fit for C.
    val interpreted = File.createTempFile("vercors-interpreted-", ".i")
    val process = interpret(localInclude=Option(f.toPath.getParent).toSeq, input=f.getAbsolutePath, output=interpreted.getAbsolutePath)
    val exit = process.waitFor()

    exit match {
      case 0 => ColIParser().parse(interpreted)()
      case errorCode =>
        val errorOutput = new Scanner(process.getErrorStream).useDelimiter("\\A" /* start of input (no delimiter) */).next()
        throw PreprocessorError(f.toPath, errorCode, errorOutput)
    }
  }
}
