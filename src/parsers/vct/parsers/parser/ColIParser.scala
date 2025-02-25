package vct.parsers.parser

import org.antlr.v4.runtime.{CharStream, CommonTokenStream, Token, TokenStream}
import vct.antlr4.generated.{CParser, LangCLexer}
import vct.col.ast.GlobalDeclaration
import vct.col.origin.{ExpectedError, Origin}
import vct.parsers.debug.DebugOptions
import vct.parsers.err.ParseMatchError
import vct.parsers.transform.{BlameProvider, CToCol}
import vct.parsers.{AntlrParser, ParseResult, Parser}

case class ColIParser(
    debugOptions: DebugOptions,
    blameProvider: BlameProvider,
    cOrigin: Option[Origin],
) extends AntlrParser {
  override type Parser = CParser
  override type Lexer = LangCLexer
  override type MainContext = CParser.CompilationUnitContext
  override type Converter[_] = CToCol[_]

  override def newLexer(charStream: CharStream): Lexer =
    new LangCLexer(charStream)
  override def expectedErrorChannel: Int = LangCLexer.EXPECTED_ERROR_CHANNEL
  override def expectedErrorStart: Int = LangCLexer.VAL_EXPECT_ERROR_OPEN
  override def expectedErrorEnd: Int = LangCLexer.VAL_EXPECT_ERROR_CLOSE

  override def newParser(tokenStream: TokenStream): Parser =
    new CParser(tokenStream)
  override def parseMain(parser: Parser): MainContext = parser.compilationUnit()

  override def newConverter[G](
      baseOrigin: Origin,
      expectedErrors: Seq[(Token, Token, ExpectedError)],
      tokenStream: TokenStream,
  ): Converter[G] =
    CToCol(
      baseOrigin,
      blameProvider,
      expectedErrors,
      cOrigin.map(o => (tokenStream, o)),
    )

  override def mainToResult[G](
      converter: Converter[G],
      main: MainContext,
  ): Seq[GlobalDeclaration[_]] = converter.convert(main)
}
