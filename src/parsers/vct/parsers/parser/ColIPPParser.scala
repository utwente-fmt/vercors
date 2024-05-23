package vct.parsers.parser

import org.antlr.v4.runtime.{CharStream, CommonTokenStream, Token, TokenStream}
import vct.antlr4.generated.{CPPParser, LangCPPLexer}
import vct.col.ast.GlobalDeclaration
import vct.col.origin.{ExpectedError, Origin}
import vct.parsers.debug.DebugOptions
import vct.parsers.err.ParseMatchError
import vct.parsers.transform.{BlameProvider, CPPToCol}
import vct.parsers.{AntlrParser, ParseResult, Parser}

case class ColIPPParser(debugOptions: DebugOptions, blameProvider: BlameProvider, cppOrigin: Option[Origin]) extends AntlrParser {
  override type Parser = CPPParser
  override type Lexer = LangCPPLexer
  override type MainContext = CPPParser.TranslationUnitContext
  override type Converter[_] = CPPToCol[_]

  override def newLexer(charStream: CharStream): Lexer = new LangCPPLexer(charStream)
  override def expectedErrorChannel: Int = LangCPPLexer.EXPECTED_ERROR_CHANNEL
  override def expectedErrorStart: Int = LangCPPLexer.VAL_EXPECT_ERROR_OPEN
  override def expectedErrorEnd: Int = LangCPPLexer.VAL_EXPECT_ERROR_CLOSE

  override def newParser(tokenStream: TokenStream): Parser = new CPPParser(tokenStream)
  override def parseMain(parser: Parser): MainContext = parser.translationUnit()

  override def newConverter[G](baseOrigin: Origin, expectedErrors: Seq[(Token, Token, ExpectedError)], tokenStream: TokenStream): Converter[G] =
    CPPToCol[G](baseOrigin, blameProvider, expectedErrors, cppOrigin.map(o => (tokenStream, o)))

  override def mainToResult[G](converter: Converter[G], main: MainContext): Seq[GlobalDeclaration[_]] = converter.convert(main)
}
