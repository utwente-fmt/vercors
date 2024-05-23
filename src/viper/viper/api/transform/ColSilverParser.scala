package viper.api.transform

import hre.io.{Readable, ReaderReadable}
import vct.col.origin.Origin
import vct.parsers.transform.BlameProvider
import vct.parsers.{ParseResult, Parser}

import java.io.Reader

case class ColSilverParser(blameProvider: BlameProvider) extends Parser {
  override def parse[G](readable: Readable, baseOrigin: Origin = Origin(Nil)): ParseResult[G] =
    ParseResult(SilverToCol.parse(readable, blameProvider).declarations, Nil)

  override def parseReader[G](reader: Reader, baseOrigin: Origin = Origin(Nil)): ParseResult[G] =
    ParseResult(SilverToCol.parse(ReaderReadable("<unknown>", reader), blameProvider).declarations, Nil)
}
