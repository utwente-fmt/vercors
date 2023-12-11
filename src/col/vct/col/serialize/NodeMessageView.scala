package vct.col.serialize

import com.google.protobuf.CodedOutputStream
import vct.col.ast.Node
import vct.serialize.StreamingOpaqueMessageBytes

class NodeMessageView(node: Node[_]) extends StreamingOpaqueMessageBytes {
  lazy val message: scalapb.GeneratedMessage = node.serializeFamily()

  override def streamTo(output: CodedOutputStream): Unit =
    message.writeTo(output)

  override def size(): Int = message.serializedSize
}
