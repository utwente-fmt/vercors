package vct.parsers

import com.typesafe.scalalogging.LazyLogging
import hre.io.Readable
import org.antlr.v4.runtime.{CharStream, CommonTokenStream}
import vct.antlr4.generated.{LLVMSpecParser, LangLLVMSpecLexer}
import vct.col.ast.serialize.Program
import vct.col.ast.{Declaration, Deserialize}
import vct.col.origin.{ExpectedError, Origin}
import vct.col.ref.Ref
import vct.parsers.transform.{BlameProvider, LLVMContractToCol, OriginProvider}
import vct.result.VerificationError.{SystemError, Unreachable, UserError}

import java.io.{InputStreamReader, StringWriter}

case class ColLLVMParser(override val origin: Origin, override val blameProvider: BlameProvider)
                                                    extends Parser(origin, blameProvider) with LazyLogging {
  case class LLVMParseError(fileName: String, errorCode: Int, error: String) extends UserError {
    override def code: String = "LLVMParseError"

    override def text: String =
      s"[ERROR] Parsing file $fileName failed with exit code $errorCode:\n$error"
  }

  override def parse[G](stream: CharStream): ParseResult[G] = {
    throw Unreachable("LLVM IR files shouldn't be parsed from an ANTLR CharStream, use VCLLVM instead!")
  }

  override def parse[G](readable: Readable): ParseResult[G] = {
    val command = Seq("VCLLVM", readable.fileName)
    val process = new ProcessBuilder(command: _*).start()
    process.waitFor()
    if (process.exitValue() != 0) {
      val writer = new StringWriter()
      new InputStreamReader(process.getInputStream).transferTo(writer)
      new InputStreamReader(process.getErrorStream).transferTo(writer)
      writer.close()
      throw LLVMParseError(readable.fileName, process.exitValue(), writer.toString)
    }
    val protoProgram = Program.parseFrom(process.getInputStream)
    val COLProgram = Deserialize.deserializeProgram[G](protoProgram, readable.fileName)
    ParseResult(COLProgram.declarations, Seq.empty)
  }

  def parseFunctionContract[G](stream: CharStream):
  (vct.col.ast.ApplicableContract[G], Seq[ExpectedError]) = {
    val lexer = new LangLLVMSpecLexer(stream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LLVMSpecParser(tokens)
    // we're parsing a contract so set the parser to specLevel == 1
    parser.specLevel = 1

    val (errors, tree) = noErrorsOrThrow(origin, parser, lexer) {
      val errors = expectedErrors(tokens, LangLLVMSpecLexer.EXPECTED_ERROR_CHANNEL, LangLLVMSpecLexer.VAL_EXPECT_ERROR_OPEN, LangLLVMSpecLexer.VAL_EXPECT_ERROR_CLOSE)
      val tree = parser.valEmbedContract()
      (errors, tree)
    }
    val contract = LLVMContractToCol[G](origin, blameProvider, errors).convert(tree)
    (contract, errors.map(_._3))
  }

  def parseGlobal[G](stream: CharStream): (vct.col.ast.GlobalDeclaration[G], Seq[ExpectedError]) = {
    val lexer = new LangLLVMSpecLexer(stream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LLVMSpecParser(tokens)
    // we're parsing a contract so set the parser to specLevel == 1
    parser.specLevel = 1

    val (errors, tree) = noErrorsOrThrow(origin, parser, lexer) {
      val errors = expectedErrors(tokens, LangLLVMSpecLexer.EXPECTED_ERROR_CHANNEL, LangLLVMSpecLexer.VAL_EXPECT_ERROR_OPEN, LangLLVMSpecLexer.VAL_EXPECT_ERROR_CLOSE)
      val tree = parser.valGlobalDeclaration()
      (errors, tree)
    }
    val global = LLVMContractToCol[G](origin, blameProvider, errors).convert(tree)
    (global, errors.map(_._3))
  }
}