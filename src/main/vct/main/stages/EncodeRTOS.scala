package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.stages.Stage
import vct.col.rewrite.{Generation, Rewritten}
import vct.options.Options
import vct.parsers.ParseResult
import vct.rewrite.rtos.RTOSEncoder

case object EncodeRTOS {
  def ofOptions[G <: Generation](options: Options): Stage[ParseResult[G], ParseResult[Rewritten[G]]] = {
    EncodeRTOS()
  }
}

case class EncodeRTOS[G <: Generation]() extends Stage[ParseResult[G], ParseResult[Rewritten[G]]] with LazyLogging {

  override def friendlyName: String =
    "Transform AST from FreeRTOS C program to PVL encoding"

  override def progressWeight: Int = 0

  override def run(in1: ParseResult[G]): ParseResult[Rewritten[G]] =
    ParseResult(RTOSEncoder.transform(in1.decls), in1.expectedErrors)
}
