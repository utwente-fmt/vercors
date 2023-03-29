package vct.parsers

import com.typesafe.scalalogging.LazyLogging
import hre.io.Readable
import org.antlr.v4.runtime.CharStream
import vct.col.ast.Deserialize
import vct.col.serialize.{GlobalDeclaration, Program}
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationError.{SystemError, Unreachable, UserError}

import java.io.{InputStreamReader, StringWriter}

case class ColLLVMParser(override val originProvider: OriginProvider, override val blameProvider: BlameProvider) extends Parser(originProvider, blameProvider) with LazyLogging {
  case class LLVMParseError(fileName: String, errorCode: Int, error: String) extends UserError {
    override def code: String = "LLVMParseError"

    override def text: String =
      s"Parsing file $fileName failed with exit code $errorCode:\n$error"
  }

  override def parse[G](stream: CharStream): ParseResult[G] = {
    throw Unreachable("LLVM IR files shouldn't be parsed from an ANTLR CharStream, use VCLLVM instead!")
  }

  override def parse[G](readable: Readable): ParseResult[G] = {
    val command = Seq("VCLLVM", readable.fileName)
    val process = new ProcessBuilder(command:_*).start()
    process.waitFor()
    if (process.exitValue() != 0) {
      val writer = new StringWriter()
      new InputStreamReader(process.getInputStream).transferTo(writer)
      new InputStreamReader(process.getErrorStream).transferTo(writer)
      writer.close()
      throw LLVMParseError(readable.fileName, process.exitValue(), writer.toString)
    }
    val protoProgram = Program.parseFrom(process.getInputStream)
    val COLProgram = Deserialize.deserialize[G](protoProgram)
    ParseResult(COLProgram.declarations, Seq.empty)
  }

}