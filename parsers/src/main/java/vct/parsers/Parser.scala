package vct.parsers

import hre.lang.System.Failure
import org.antlr.v4.runtime
import vct.col.ast.stmt.decl.ProgramUnit

import java.io.{File, FileInputStream, FileNotFoundException, InputStream}

abstract class Parser {
  def parse(stream: runtime.CharStream, name: String): ProgramUnit

  def parse(f: File): ProgramUnit = {
    val name = f.toString
    try {
      parse(new FileInputStream(f), name)
    } catch {
      case _: FileNotFoundException =>
        throw Failure("Could not find file: %s", name)
    }
  }

  def parse(stream: InputStream, name: String): ProgramUnit =
    parse(runtime.CharStreams.fromStream(stream), name)

  protected def errorCounter(parser: runtime.Parser, lexer: runtime.Lexer, name: String): ErrorCounter = {
    parser.removeErrorListeners()
    lexer.removeErrorListeners()
    val ec = new ErrorCounter(name)
    parser.addErrorListener(ec)
    lexer.addErrorListener(ec)
    ec
  }
}
