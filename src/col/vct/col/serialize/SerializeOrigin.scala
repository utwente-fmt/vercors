package vct.col.serialize

import vct.col.ast.{serialize => ser}
import vct.col.origin._

import scala.annotation.unused

object SerializeOrigin {
  def deserialize(
      @unused
      origin: ser.Origin
  ): Origin =
    Origin(origin.content.map(_.content).map {
      case ser.OriginContent.Content.SourceName(name) => SourceName(name.name)
      case ser.OriginContent.Content.PreferredName(name) =>
        PreferredName(name.preferredName)
      case ser.OriginContent.Content.Context(context) =>
        DeserializedContext(
          context.context,
          context.inlineContext,
          context.shortPosition,
        )
    })

  def serialize(
      @unused
      origin: Origin
  ): ser.Origin =
    ser.Origin(
      origin.originContents.flatMap {
        case SourceName(name) =>
          Seq(ser.OriginContent.Content.SourceName(ser.SourceName(name)))
        case PreferredName(preferredName) =>
          Seq(
            ser.OriginContent.Content
              .PreferredName(ser.PreferredName(preferredName))
          )
        case DeserializedContext(context, inlineContext, shortPosition) =>
          Seq(
            ser.OriginContent.Content
              .Context(ser.Context(context, inlineContext, shortPosition))
          )
        case _ => Nil
      }.map(ser.OriginContent(_))
    )
}

case class DeserializedContext(
    context: String,
    inlineContext: String,
    shortPosition: String,
) extends Context {
  override protected def contextHere(tail: Origin): (String, Origin) =
    (context, tail)
  override protected def inlineContextHere(
      tail: Origin,
      compress: Boolean = true,
  ): (String, Origin) = (inlineContext, tail)
  override protected def shortPositionHere(tail: Origin): (String, Origin) =
    (shortPosition, tail)
}
