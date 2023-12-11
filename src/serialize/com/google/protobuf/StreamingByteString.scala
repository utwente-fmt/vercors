package com.google.protobuf

import java.io.ByteArrayOutputStream

/**
 * ForwardingByteString that optimizes for writing literal generated messages.
 */
trait StreamingByteString { this: ForwardingByteString =>
  override lazy val underlying: ByteString = {
    val buf = new Array[Byte](size())
    val out = CodedOutputStream.newInstance(buf)
    this.streamTo(out)
    ByteString.wrap(buf)
  }

  override def size(): Int

  def streamTo(output: CodedOutputStream): Unit

  override def writeTo(out: ByteOutput): Unit =
    out match {
      case out: CodedOutputStream => streamTo(out)
      case other => CodedOutputStream.newInstance(other, CodedOutputStream.DEFAULT_BUFFER_SIZE)
    }
}
