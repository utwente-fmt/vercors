package vct.parsers

import java.io.{File, FileInputStream, FileNotFoundException, InputStream}
import hre.lang.System.Failure
import org.antlr.v4.runtime
import vct.col.ast.GlobalDeclaration
import vct.col.ast.stmt.decl.ProgramUnit
import vct.parsers.transform.{BlameProvider, FileOriginProvider, OriginProvider}

abstract class Parser {
  def parse(stream: runtime.CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): Seq[GlobalDeclaration]

  def parse(stream: InputStream, originProvider: OriginProvider, blameProvider: BlameProvider): Seq[GlobalDeclaration] =
    parse(runtime.CharStreams.fromStream(stream), originProvider, blameProvider)

  def parse(f: File)(originProvider: OriginProvider = FileOriginProvider(f.toPath),
                     blameProvider: BlameProvider = FileOriginProvider(f.toPath)): Seq[GlobalDeclaration] = {
    val name = f.toString
    try {
      parse(new FileInputStream(f), originProvider, blameProvider)
    } catch {
      case _: FileNotFoundException =>
        throw Failure("Could not find file: %s", name)
    }
  }

  protected def errorCounter(parser: runtime.Parser, lexer: runtime.Lexer): ErrorCounter = {
    parser.removeErrorListeners()
    lexer.removeErrorListeners()
    val ec = new ErrorCounter(???)
    parser.addErrorListener(ec)
    lexer.addErrorListener(ec)
    ec
  }
}
