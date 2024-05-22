package vct.serialize

import com.google.protobuf.{ByteString, ForwardingByteString}
import scalapb.{GeneratedMessage, TypeMapper}

object OpaqueMessageBytes {
  implicit val protobufBimap: TypeMapper[ByteString, OpaqueMessageBytes] =
    TypeMapper(
      (bs: ByteString) => (new LiteralOpaqueMessageBytes(bs): OpaqueMessageBytes)
    )(
      identity[OpaqueMessageBytes]
    )
}

abstract class OpaqueMessageBytes extends ForwardingByteString {
  def parseAs[T <: GeneratedMessage](implicit companion: scalapb.GeneratedMessageCompanion[T]): T =
    companion.parseFrom(newCodedInput())
}
