package viper.api.transform

import hre.io.Readable
import org.antlr.v4.runtime.CharStream
import vct.col.origin.Origin
import vct.parsers.transform.{BlameProvider, OriginProvider}
import vct.parsers.{ParseResult, Parser}
import vct.result.VerificationError.Unreachable

case class ColSilverParser(override val origin: Origin,
                           override val blameProvider: BlameProvider) extends Parser(origin, blameProvider) {
  override def parse[G](stream: CharStream): ParseResult[G] =
    throw Unreachable("Should not parse silver files from CharStream: Viper is not parsed via ANTLR.")

  override def parse[G](readable: Readable): ParseResult[G] =
    ParseResult(SilverToCol.parse(readable, blameProvider).declarations, Nil)
}
