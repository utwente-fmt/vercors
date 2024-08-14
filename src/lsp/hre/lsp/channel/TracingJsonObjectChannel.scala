package hre.lsp.channel
import ujson.Obj

import java.io.BufferedOutputStream

case class TracingJsonObjectChannel(
    inner: JsonObjectChannel,
    traceRead: BufferedOutputStream,
    traceWrite: BufferedOutputStream,
) extends JsonObjectChannel {
  override def read(): Option[Obj] =
    inner.read() match {
      case None =>
        traceRead.write("End of stream\n".getBytes)
        traceRead.flush()
        None
      case Some(obj) =>
        obj.writeBytesTo(traceRead)
        traceRead.flush()
        Some(obj)
    }

  override def write(obj: Obj): Unit = {
    obj.writeBytesTo(traceWrite)
    traceWrite.flush()
    inner.write(obj)
  }
}
