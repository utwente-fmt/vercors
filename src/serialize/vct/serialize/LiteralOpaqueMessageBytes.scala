package vct.serialize

import com.google.protobuf.ByteString

class LiteralOpaqueMessageBytes(override val underlying: ByteString) extends OpaqueMessageBytes {
  override def size(): Int =
    underlying.size()
}
