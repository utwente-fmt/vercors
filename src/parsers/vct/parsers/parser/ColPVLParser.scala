package vct.parsers.parser

import org.antlr.v4.runtime.{CharStream, CommonTokenStream, Token, TokenStream}
import vct.antlr4.generated.{LangPVLLexer, PVLParser}
import vct.col.ast.GlobalDeclaration
import vct.col.origin.{ExpectedError, Origin}
import vct.parsers.debug.DebugOptions
import vct.parsers.err.ParseMatchError
import vct.parsers.transform.{BlameProvider, PVLToCol}
import vct.parsers.{AntlrParser, ParseResult, Parser}

case class ColPVLParser(debugOptions: DebugOptions, blameProvider: BlameProvider) extends AntlrParser {
  override type Parser = PVLParser
  override type Lexer = LangPVLLexer
  override type MainContext = PVLParser.ProgramContext
  override type Converter[_] = PVLToCol[_]

  override def newLexer(charStream: CharStream): Lexer = new LangPVLLexer(charStream)
  override def expectedErrorChannel: Int = LangPVLLexer.EXPECTED_ERROR_CHANNEL
  override def expectedErrorStart: Int = LangPVLLexer.VAL_EXPECT_ERROR_OPEN
  override def expectedErrorEnd: Int = LangPVLLexer.VAL_EXPECT_ERROR_CLOSE

  override def newParser(tokenStream: TokenStream): Parser = new PVLParser(tokenStream)
  override def parseMain(parser: Parser): MainContext = parser.program()

  override def newConverter[G](baseOrigin: Origin, expectedErrors: Seq[(Token, Token, ExpectedError)], tokenStream: TokenStream): Converter[G] =
    new PVLToCol[G](baseOrigin, blameProvider, expectedErrors)

  override def mainToResult[G](converter: Converter[G], main: MainContext): Seq[GlobalDeclaration[_]] =
    converter.convert(main)
}
