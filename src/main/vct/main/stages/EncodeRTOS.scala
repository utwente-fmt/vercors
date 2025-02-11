package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.stages.Stage
import vct.col.rewrite.{Generation, Rewritten}
import vct.options.Options
import vct.parsers.ParseResult
import vct.rewrite.rtos.RTOSEncoder

case object EncodeRTOS {
  def ofOptions(options: Options): Stage[ParseResult[_ <: Generation], ParseResult[_ <: Generation]] = {
    EncodeRTOS()
  }
}

case class EncodeRTOS() extends Stage[ParseResult[_ <: Generation], ParseResult[_ <: Generation]] with LazyLogging {

  override def friendlyName: String =
    "Transform AST from FreeRTOS C program to PVL encoding"

  override def progressWeight: Int = 0

  override def run(in: ParseResult[_ <: Generation]): ParseResult[_ <: Generation] =
    ParseResult(RTOSEncoder.transform(in.decls), in.expectedErrors)
}
