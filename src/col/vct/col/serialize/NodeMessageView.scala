package vct.col.serialize

import com.google.protobuf.CodedOutputStream
import vct.col.ast.{Declaration, Node}
import vct.serialize.StreamingOpaqueMessageBytes

class NodeMessageView[G](node: Node[G], decls: Map[Declaration[G], Long]) extends StreamingOpaqueMessageBytes {
  lazy val message: scalapb.GeneratedMessage = node.serializeFamily(decls)

  override def streamTo(output: CodedOutputStream): Unit =
    message.writeTo(output)

  override def size(): Int = message.serializedSize
}
