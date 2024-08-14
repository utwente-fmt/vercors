package hre.lsp.channel

import ujson.Obj

import java.io.{BufferedInputStream, BufferedOutputStream, IOException}
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

case class LspCarrierChannel(in: BufferedInputStream, out: BufferedOutputStream)
    extends JsonObjectChannel with Iterable[ujson.Obj] {
  private def readLine(): Option[String] = {
    val buf = ArrayBuffer[Byte]()

    var c = in.read()

    if (c == -1)
      return None

    while (c != 0xd && c != -1) {
      buf += c.toByte
      c = in.read()
    }

    if (c != 0xd || in.read() != 0xa) {
      throw new IOException(
        "jsonrpc protocol error: invalid line ending or unexpected end of stream"
      )
    }

    Some(new String(buf.toArray, StandardCharsets.US_ASCII))
  }

  override def read(): Option[Obj] = {
    val headers = mutable.Map[String, String]()

    var line = readLine().getOrElse(return None)

    while (line.nonEmpty) {
      line.split(":", 2) match {
        case Array(key, value) =>
          headers(key.strip().toLowerCase) = value.strip()
        case _ =>
          throw new IOException(
            s"jsonrpc protocol error: malformed header line: ${line}"
          )
      }

      line = readLine().getOrElse(
        throw new IOException(
          "jsonrpc protocol error: unexpected end of stream"
        )
      )
    }

    if (!headers.contains("content-length"))
      throw new IOException(
        "jsonrpc protocol error: no content-length in the header"
      )

    // Currently the only supported encoding in the LSP specification is UTF-8, so just ignore content-type.

    val length = Try(Integer.parseInt(headers("content-length"))).getOrElse(
      throw new IOException(
        "jsonrpc protocol error: content-length is not a number"
      )
    )

    val data = in.readNBytes(length)

    if (data.length < length)
      throw new IOException("jsonrpc protocol error: unexpected end of stream")

    val obj =
      Try(ujson.read(data)).recover(t =>
        throw new IOException(s"jsonrpc protocol error: ${t.getMessage}")
      ).get

    obj match {
      case obj: ujson.Obj => Some(obj)
      case other =>
        throw new IOException(
          s"jsonrpc protocol error: expected Obj, got ${other.getClass.getSimpleName}"
        )
    }
  }

  override def write(obj: Obj): Unit = {
    val data = ujson.writeToByteArray(obj)
    out.write(
      s"Content-Length: ${data.length}\r\n\r\n"
        .getBytes(StandardCharsets.US_ASCII)
    )
    out.write(data)
    out.flush()
  }

  override def iterator: Iterator[Obj] =
    new Iterator[Obj] {
      private var buf: Option[Obj] = None

      def tryEnsure(): Unit = if (buf.isEmpty) { buf = read() }

      override def hasNext: Boolean = {
        tryEnsure()
        buf.isDefined
      }

      override def next(): Obj = {
        tryEnsure()
        val result = buf.get
        buf = None
        result
      }
    }
}
