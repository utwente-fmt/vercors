package vct.col.rewrite.backend

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter}

case object EncodeHeap extends ImportADTBuilder("heap")

case class EncodeHeap[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  private lazy val refFile = parse("ref")

  private lazy val refAdt = find[AxiomaticDataType[Post]](refFile, "ref")
  private lazy val refNull = find[ADTFunction[Post]](refAdt, "null")
}
