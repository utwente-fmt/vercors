package vct.serialize

import com.google.protobuf.{ByteString, CodedOutputStream, StreamingByteString}

import java.io.ByteArrayOutputStream

abstract class StreamingOpaqueMessageBytes extends OpaqueMessageBytes with StreamingByteString {
  override def streamTo(output: CodedOutputStream): Unit
  override def size(): Int
}