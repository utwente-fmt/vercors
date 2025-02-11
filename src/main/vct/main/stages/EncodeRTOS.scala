package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.stages.Stage
import vct.col.rewrite.Generation
import vct.options.Options
import vct.parsers.ParseResult
import vct.rewrite.rtos.RTOSEncoder

case object EncodeRTOS {
  def ofOptions[G](options: Options): Stage[ParseResult[_ <: Generation], ParseResult[G]] = {
    EncodeRTOS()
  }
}

case class EncodeRTOS[G]() extends Stage[ParseResult[_ <: Generation], ParseResult[G]] with LazyLogging {

  override def friendlyName: String =
    "Transform AST from FreeRTOS C program to PVL encoding"

  override def progressWeight: Int = 0

  override def run(in1: ParseResult[_ <: Generation]): ParseResult[G] =
    ParseResult(RTOSEncoder.transform(in1.decls), in1.expectedErrors)
}
