package vct.parsers.parser

import com.google.protobuf.InvalidProtocolBufferException
import com.typesafe.scalalogging.LazyLogging
import hre.io.Readable
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{LLVMSpecParser, LangLLVMSpecLexer}
import vct.col.ast.serialize.Program
import vct.col.ast.{Declaration, Deserialize, LLVMFunctionDefinition}
import vct.col.origin.{ExpectedError, Origin}
import vct.col.ref.Ref
import vct.parsers.transform.{BlameProvider, LLVMContractToCol, OriginProvider}
import vct.result.VerificationError.{SystemError, Unreachable, UserError}
import vct.parsers.{Parser, ParseResult}
import vct.parsers.debug.DebugOptions

import java.io.{IOException, Reader}
import java.nio.file.Path
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.util.{Failure, Using}

case class ColLLVMParser(
    debugOptions: DebugOptions,
    blameProvider: BlameProvider,
    pallas: Path,
) extends Parser with LazyLogging {
  private case class LLVMParseError(
      fileName: String,
      errorCode: Int,
      error: String,
  ) extends UserError {
    override def code: String = "LLVMParseError"

    override def text: String =
      messageContext(
        s"[ERROR] Parsing file $fileName failed with exit code $errorCode:\n$error"
      )
  }

  override def parse[G](
      readable: Readable,
      baseOrigin: Origin = Origin(Nil),
  ): ParseResult[G] = {
    if (pallas == null) {
      throw Unreachable(
        "The ColLLVMParser needs to be provided with the path to pallas to parse LLVM-IR files"
      )
    }
    val command = Seq(
      "opt-17",
      s"--load-pass-plugin=${pallas.toAbsolutePath}",
      "--passes=module(pallas-declare-variables,pallas-collect-module-spec),function(pallas-declare-function,pallas-assign-pure,pallas-declare-function-contract,pallas-transform-function-body),module(pallas-print-protobuf)",
      readable.fileName,
      "--disable-output",
    )

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
            ).indent(8),
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
        ).indent(8),
      )
    }

    // Use the origin in the blame provider
    val COLProgram = Deserialize
      .deserializeProgram[G](protoProgram, _ => blameProvider.apply())
    ParseResult(COLProgram.declarations, Seq.empty)
  }

  override def parseReader[G](
      reader: Reader,
      baseOrigin: Origin,
  ): ParseResult[G] = throw new UnsupportedOperationException()
}
