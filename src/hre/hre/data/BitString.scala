package hre.data

object BitString {
  def apply(input: String): BitString = {
    val bytes =
      if (input.length % 8 == 0)
        input.length / 8
      else
        input.length / 8 + 1
    val skip =
      if (input.length % 8 == 0)
        0
      else
        8 - (input.length % 8)
    val buf = new Array[Byte](bytes)
    for (i <- 0 until bytes) {
      val value =
        (if (input(i * 8 + 0) == '1')
           -128
         else
           0) +
          (if (input(i * 8 + 1) == '1')
             64
           else
             0) +
          (if (input(i * 8 + 2) == '1')
             32
           else
             0) +
          (if (input(i * 8 + 3) == '1')
             16
           else
             0) +
          (if (input(i * 8 + 4) == '1')
             8
           else
             0) +
          (if (input(i * 8 + 5) == '1')
             4
           else
             0) +
          (if (input(i * 8 + 6) == '1')
             2
           else
             0) +
          (if (input(i * 8 + 7) == '1')
             1
           else
             0)

      buf(i) = value.toByte
    }
    new BitString(buf, skip)
  }
}

class BitString(val rawData: Array[Byte], val skipAtLastByte: Int)
    extends Seq[Boolean] {
  bitString =>

  require(0 <= skipAtLastByte && skipAtLastByte < 8)

  def toSmt: String =
    if (skipAtLastByte == 0 || skipAtLastByte == 4)
      toHexSmt
    else
      toBinSmt

  def toHexSmt: String = {
    val digit = "0123456789abcdef"
    val sb = new StringBuilder("#x")

    for (i <- 0 until rawData.length - 1) {
      val b = rawData(i)
      sb += digit((b >> 4) & 0xf)
      sb += digit((b >> 0) & 0xf)
    }

    if (rawData.length > 0) {
      val b = rawData.last

      if (skipAtLastByte == 0) {
        sb += digit((b >> 4) & 0xf)
        sb += digit((b >> 0) & 0xf)
      } else { sb += digit((b >> 4) & 0xf) }
    }

    sb.result()
  }

  def toBinSmt: String = {
    val bit = "01"
    val sb = new StringBuilder("#b")

    for (i <- 0 until rawData.length - 1) {
      val b = rawData(i)
      sb += bit((b >> 7) & 1)
      sb += bit((b >> 6) & 1)
      sb += bit((b >> 5) & 1)
      sb += bit((b >> 4) & 1)
      sb += bit((b >> 3) & 1)
      sb += bit((b >> 2) & 1)
      sb += bit((b >> 1) & 1)
      sb += bit((b >> 0) & 1)
    }

    if (rawData.length > 0) {
      val b = rawData.last
      for (offset <- (skipAtLastByte until 8).reverse) {
        sb += bit((b >> offset) & 1)
      }
    }

    sb.result()
  }

  override def apply(i: Int): Boolean = {
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i)

    (rawData(i / 8) >> (i % 8)) > 0
  }

  override def length: Int = rawData.length * 8 - skipAtLastByte

  class BitStringIterator extends Iterator[Boolean] {
    private var index: Int = 0

    override def hasNext: Boolean = index < bitString.length
    override def next(): Boolean =
      try { bitString(index) }
      finally { index += 1 }
  }

  override def iterator: Iterator[Boolean] = new BitStringIterator
}
