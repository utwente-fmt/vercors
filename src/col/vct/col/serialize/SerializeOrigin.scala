package vct.col.serialize

import hre.io.{ChecksumReadableFile, RWFile}
import vct.col.ast.{serialize => ser}
import vct.col.origin._

import java.nio.file.Path
import scala.annotation.unused
import scala.collection.mutable
import com.typesafe.scalalogging.LazyLogging;

object SerializeOrigin extends LazyLogging {
  private def fileMap: mutable.HashMap[Path, hre.io.Readable] =
    mutable.HashMap()

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
      case ser.OriginContent.Content.ReadableOrigin(context) =>
        val path = Path.of(context.directory, context.filename)
        ReadableOrigin(fileMap.getOrElseUpdate(
          path, {
            if (context.checksum.isDefined && context.checksumKind.isDefined) {
              val file = ChecksumReadableFile(
                path,
                doWatch = false,
                context.checksumKind.get,
              )
              if (file.getChecksum != context.checksum.get) {
                logger.warn(
                  "The checksum of the file " + path +
                    " does not match the LLVM checksum error locations are likely inaccurate"
                )
              }
              file
            } else { RWFile(path) }
          },
        ))
      case ser.OriginContent.Content.PositionRange(range) =>
        // TODO: Preserve the start col idx even if end col idx is missing and improve the origins in LangLLVMToCol? Maybe we could even set the preferred name correctly
        PositionRange(
          range.startLineIdx,
          range.endLineIdx,
          range.startColIdx.flatMap { start => range.endColIdx.map((start, _)) },
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
