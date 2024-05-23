package vct.parsers.parser

import com.typesafe.scalalogging.LazyLogging
import hre.io.Readable
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{LLVMSpecParser, LangLLVMSpecLexer}
import vct.col.ast.Deserialize
import vct.col.ast.serialize.Program
import vct.col.origin.{ExpectedError, Origin}
import vct.parsers.debug.DebugOptions
import vct.parsers.transform.{BlameProvider, LLVMContractToCol}
import vct.parsers.{ParseResult, Parser}
import vct.result.VerificationError.{Unreachable, UserError}

import java.io.{IOException, Reader}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.util.{Failure, Using}

case class ColLLVMParser(
    debugOptions: DebugOptions,
    blameProvider: BlameProvider,
    vcllvm: Path,
) extends Parser with LazyLogging {
  case class LLVMParseError(fileName: String, errorCode: Int, error: String)
      extends UserError {
    override def code: String = "LLVMParseError"

    override def text: String =
      s"[ERROR] Parsing file $fileName failed with exit code $errorCode:\n$error"
  }

  override def parse[G](
      readable: Readable,
      baseOrigin: Origin = Origin(Nil),
  ): ParseResult[G] = {
    if (vcllvm == null) {
      throw Unreachable(
        "The COLLVMParser needs to be provided with the path to vcllvm to parse LLVM-IR files"
      )
    }
    val command = Seq(vcllvm.toString, readable.fileName)
    val process = new ProcessBuilder(command: _*).start()

    val protoProgram =
      Using(process.getInputStream) { is => Program.parseFrom(is) }
        .recoverWith { case _: IOException =>
          Failure(LLVMParseError(
            readable.fileName,
            process.exitValue(),
            new String(
              process.getErrorStream.readAllBytes(),
              StandardCharsets.UTF_8,
            ),
          ))
        }.get

    process.waitFor()
    if (process.exitValue() != 0) {
      throw LLVMParseError(
        readable.fileName,
        process.exitValue(),
        new String(
          process.getErrorStream.readAllBytes(),
          StandardCharsets.UTF_8,
        ),
      )
    }

    val COLProgram = Deserialize
      .deserializeProgram[G](protoProgram, readable.fileName)
    ParseResult(COLProgram.declarations, Seq.empty)
  }

  override def parseReader[G](
      reader: Reader,
      baseOrigin: Origin,
  ): ParseResult[G] = throw new UnsupportedOperationException()
}
