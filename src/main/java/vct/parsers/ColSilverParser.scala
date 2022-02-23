package vct.parsers
import org.antlr.v4.runtime.CharStream
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.result.VerificationResult.Unreachable
import viper.api.SilverToCol

import java.io.File

case class ColSilverParser() extends Parser {
  override def parse[G](stream: CharStream, originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] =
    // Conceivably we can support streams via FastParser.importProgram
    throw Unreachable("Should not parse silver files from CharStream: Viper only accepts bare files.")

  override def parse[G](f: File)(originProvider: OriginProvider, blameProvider: BlameProvider): ParseResult[G] =
    ParseResult(SilverToCol.parse(f.toPath).declarations, Nil)
}
