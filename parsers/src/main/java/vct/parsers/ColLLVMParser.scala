package vct.parsers

import com.typesafe.scalalogging.LazyLogging
import hre.io.Readable
import org.antlr.v4.runtime.CharStream
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationError.{SystemError, Unreachable, UserError}

import java.io.{InputStreamReader, OutputStreamWriter, StringWriter}
import java.nio.charset.StandardCharsets

case class ColLLVMParser(override val originProvider: OriginProvider, override val blameProvider: BlameProvider) extends Parser(originProvider, blameProvider) with LazyLogging {
  case class LLVMParseError(fileName: String, errorCode: Int, error: String) extends UserError {
    override def code: String = "LLVMParseError"

    override def text: String =
      s"Parsing file $fileName failed with exit code $errorCode:\n$error"
  }

  case class LLVMStubError(message: String) extends SystemError {
    override def text: String = message
  }

  override def parse[G](stream: CharStream): ParseResult[G] = {
    throw Unreachable("LLVM IR files should be parsed from an not ANTLR CharStream, use VCLLVM instead!")
  }

  override def parse[G](readable: Readable): ParseResult[G] = {
    var command = Seq("VCLLVM", "--sample-col") //TODO convert --sample-col to actual file at some point
    val process = new ProcessBuilder(command:_*).start()
    process.waitFor()
    if (process.exitValue() != 0) {
      val writer = new StringWriter()
      new InputStreamReader(process.getInputStream).transferTo(writer)
      new InputStreamReader(process.getErrorStream).transferTo(writer)
      writer.close()
      throw LLVMParseError(readable.fileName, process.exitValue(), writer.toString)
    }
    //TODO implement Protobuf -> ParseResult
    val writer = new StringWriter()
    new InputStreamReader(process.getInputStream).transferTo(writer)
    writer.close()
    val buffer = writer.toString
    throw LLVMStubError(s"Protobuf interpreter not yet implemented. Dumping buffer...\n$buffer")
  }

}