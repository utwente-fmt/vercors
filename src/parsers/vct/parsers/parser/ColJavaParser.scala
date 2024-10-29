package vct.parsers.parser

import org.antlr.v4.runtime.{CharStream, CommonTokenStream, Token, TokenStream}
import vct.antlr4.generated.{JavaParser, LangJavaLexer}
import vct.col.ast.{Expr, GlobalDeclaration}
import vct.col.origin.{ExpectedError, Origin}
import vct.parsers.debug.DebugOptions
import vct.parsers.err.ParseMatchError
import vct.parsers.transform.{BlameProvider, JavaToCol}
import vct.parsers.{AntlrParser, ParseResult, Parser}
import hre.io.Readable

case class ColJavaParser(
    debugOptions: DebugOptions,
    blameProvider: BlameProvider,
    specCommentsNeeded: Boolean = true,
) extends AntlrParser {
  override type Parser = JavaParser
  override type Lexer = LangJavaLexer
  override type MainContext = JavaParser.CompilationUnitContext
  override type Converter[_] = JavaToCol[_]

  override def newLexer(charStream: CharStream): Lexer =
    new LangJavaLexer(charStream)
  override def expectedErrorChannel: Int = LangJavaLexer.EXPECTED_ERROR_CHANNEL
  override def expectedErrorStart: Int = LangJavaLexer.VAL_EXPECT_ERROR_OPEN
  override def expectedErrorEnd: Int = LangJavaLexer.VAL_EXPECT_ERROR_CLOSE

  override def newParser(tokenStream: TokenStream): Parser = {
    val parser = new JavaParser(tokenStream)

    if (!specCommentsNeeded)
      parser.specLevel = 1

    parser
  }
  override def parseMain(parser: Parser): MainContext = parser.compilationUnit()

  override def newConverter[G](
      baseOrigin: Origin,
      expectedErrors: Seq[(Token, Token, ExpectedError)],
      tokenStream: TokenStream,
  ): Converter[G] = JavaToCol[G](baseOrigin, blameProvider, expectedErrors)

  override def mainToResult[G](
      converter: Converter[G],
      main: JavaParser.CompilationUnitContext,
  ): Seq[GlobalDeclaration[_]] = converter.convert(main)

  def parseExpr[G](readable: Readable): (Expr[G], Seq[ExpectedError]) =
    parseOrThrow[JavaParser.ExprContext, G, Expr[G]](readable)(
      p => p.expr(),
      (cv: Converter[G], e) => cv.convert(e).asInstanceOf[Expr[G]],
    )
}
