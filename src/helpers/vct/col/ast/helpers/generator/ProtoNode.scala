package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Proto
import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoNode extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    Using(Files.newBufferedWriter(out.resolve(node.name.base + ".proto"), StandardCharsets.UTF_8)) { writer =>
      for((_, t) <- node.fields) {
        writer.write(Proto.getType(t).toString)
        writer.write("\n")
      }
    }
}
