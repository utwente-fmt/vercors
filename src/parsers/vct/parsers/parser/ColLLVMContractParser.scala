package vct.parsers.parser

import org.antlr.v4.runtime.{CharStream, Token, TokenStream}
import vct.antlr4.generated.{LLVMSpecParser, LangLLVMSpecLexer}
import vct.col.ast.{ApplicableContract, GlobalDeclaration}
import vct.col.origin.{ExpectedError, Origin}
import vct.parsers.AntlrParser
import vct.parsers.debug.DebugOptions
import vct.parsers.transform.{BlameProvider, LLVMContractToCol}
import hre.io.Readable

import java.io.Reader

case class ColLLVMContractParser(debugOptions: DebugOptions, blameProvider: BlameProvider) extends AntlrParser {
  override type Parser = LLVMSpecParser
  override type Lexer = LangLLVMSpecLexer
  override type MainContext = LLVMSpecParser.ValGlobalDeclarationContext
  override type Converter[_] = LLVMContractToCol[_]

  override def newLexer(charStream: CharStream): Lexer = new LangLLVMSpecLexer(charStream)
  override def expectedErrorChannel: Int = LangLLVMSpecLexer.EXPECTED_ERROR_CHANNEL
  override def expectedErrorStart: Int = LangLLVMSpecLexer.VAL_EXPECT_ERROR_OPEN
  override def expectedErrorEnd: Int = LangLLVMSpecLexer.VAL_EXPECT_ERROR_CLOSE

  override def newParser(tokenStream: TokenStream): Parser = new LLVMSpecParser(tokenStream)
  override def parseMain(parser: Parser): MainContext = parser.valGlobalDeclaration()

  override def newConverter[G](baseOrigin: Origin, expectedErrors: Seq[(Token, Token, ExpectedError)], tokenStream: TokenStream): Converter[G] =
    new LLVMContractToCol[G](baseOrigin, blameProvider, expectedErrors)

  override def mainToResult[G](converter: Converter[G], main: MainContext): Seq[GlobalDeclaration[_]] =
    Seq(converter.convert(main))

  def parseFunctionContract[G](reader: Reader, baseOrigin: Origin = Origin(Nil)): (ApplicableContract[G], Seq[ExpectedError]) =
    parseReaderOrThrow[LLVMSpecParser.ValEmbedContractContext, G, ApplicableContract[G]](reader, baseOrigin)(_.valEmbedContract(), _.convert(_).asInstanceOf[ApplicableContract[G]])
}
