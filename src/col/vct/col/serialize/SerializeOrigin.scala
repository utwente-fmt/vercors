package vct.col.serialize

import vct.col.ast.{serialize => ser}
import vct.col.origin._

import scala.annotation.unused

object SerializeOrigin {
  def deserialize(@unused origin: ser.Origin): Origin =
    Origin(origin.content.map(_.content).map {
      case ser.OriginContent.Content.SourceName(name) => SourceName(name.name)
      case ser.OriginContent.Content.PreferredName(name) => PreferredName(name.preferredName)
    })

  def serialize(@unused origin: Origin): ser.Origin =
    ser.Origin(origin.originContents.flatMap {
      case SourceName(name) => Seq(ser.OriginContent.Content.SourceName(ser.SourceName(name)))
      case PreferredName(preferredName) => Seq(ser.OriginContent.Content.PreferredName(ser.PreferredName(preferredName)))
      case _ => Nil
    }.map(ser.OriginContent(_)))
}
