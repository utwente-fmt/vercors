package com.google.protobuf
import java.io.{InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util

/**
 * Utility class that forwards all calls to an underlying ByteString
 * Idea: either underlying is lazily computed so some convenient methods
 * can be specialized efficiently, or this class just wraps an already existing
 * ByteString.
 */
abstract class ForwardingByteString extends ByteString {
  def underlying: ByteString

  override def byteAt(index: Int): Byte =
    underlying.byteAt(index)

  override def internalByteAt(index: Int): Byte =
    underlying.internalByteAt(index)

  override def substring(beginIndex: Int, endIndex: Int): ByteString =
    underlying.substring(beginIndex, endIndex)

  override def copyToInternal(target: Array[Byte], sourceOffset: Int, targetOffset: Int, numberToCopy: Int): Unit =
    underlying.copyToInternal(target, sourceOffset, targetOffset, numberToCopy)

  override def copyTo(target: ByteBuffer): Unit =
    underlying.copyTo(target)

  override def writeTo(out: OutputStream): Unit =
    underlying.writeTo(out)

  override def writeTo(out: ByteOutput): Unit =
    underlying.writeTo(out)

  override def writeToInternal(out: OutputStream, sourceOffset: Int, numberToWrite: Int): Unit =
    underlying.writeTo(out, sourceOffset, numberToWrite)

  override def writeToReverse(byteOutput: ByteOutput): Unit =
    underlying.writeToReverse(byteOutput)

  override def asReadOnlyByteBuffer(): ByteBuffer =
    underlying.asReadOnlyByteBuffer()

  override def asReadOnlyByteBufferList(): util.List[ByteBuffer] =
    underlying.asReadOnlyByteBufferList()

  override def toStringInternal(charset: Charset): String =
    underlying.toStringInternal(charset)

  override def isValidUtf8: Boolean =
    underlying.isValidUtf8

  override def partialIsValidUtf8(state: Int, offset: Int, length: Int): Int =
    underlying.partialIsValidUtf8(state, offset, length)

  override def newInput(): InputStream =
    underlying.newInput()

  override def newCodedInput(): CodedInputStream =
    underlying.newCodedInput()

  override def getTreeDepth: Int =
    underlying.getTreeDepth

  override def isBalanced: Boolean =
    underlying.isBalanced

  override def partialHash(h: Int, offset: Int, length: Int): Int =
    underlying.partialHash(h, offset, length)

  override def equals(obj: scala.Any): Boolean =
    underlying.equals(obj)
}
