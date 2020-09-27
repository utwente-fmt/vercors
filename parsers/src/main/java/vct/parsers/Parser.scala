package vct.parsers

import java.io.{File, FileInputStream, FileNotFoundException, InputStream}

import hre.lang.System.Failure
import org.antlr.v4.runtime
import vct.col.ast.stmt.decl.ProgramUnit

abstract class Parser {
  def parse(stream: runtime.CharStream, name: String): ProgramUnit

  def parse(stream: InputStream, name: String): ProgramUnit =
    parse(runtime.CharStreams.fromStream(stream), name)

  def parse(f: File): ProgramUnit = {
    val name = f.toString
    try {
      parse(new FileInputStream(f), name)
    } catch {
      case _: FileNotFoundException =>
        throw Failure("Could not find file: %s", name)
    }
  }
}
