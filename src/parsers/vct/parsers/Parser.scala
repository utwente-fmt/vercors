package vct.parsers

import hre.io.Readable
import org.antlr.v4.runtime
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token}
import vct.col.origin.{ExpectedError, Origin, ReadableOrigin}
import vct.parsers.debug.{ATNTools, DebugOptions}
import vct.parsers.err.{FileNotFound, ParseError, ParseErrorStrategy, ParseErrors}
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationError.UserError

import java.io.{FileNotFoundException, Reader}
import java.nio.file.NoSuchFileException
import scala.jdk.CollectionConverters._

abstract class Parser {
  def parseReader[G](reader: Reader, baseOrigin: Origin = Origin(Nil)): ParseResult[G]

  def parse[G](readable: Readable, baseOrigin: Origin = Origin(Nil)): ParseResult[G] =
    try {
      readable.read(parseReader(_, baseOrigin.withContent(ReadableOrigin(readable))))
    } catch {
      case _: FileNotFoundException | _: NoSuchFileException => throw FileNotFound(readable.fileName)
    }
}