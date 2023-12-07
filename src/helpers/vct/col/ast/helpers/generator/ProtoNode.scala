package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoNode extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit = {
    val typeResults = node.fields.map(_._2).map(ProtoNaming.getType)
    val imports = typeResults.flatMap(r => r.imports ++ r.auxs.map(_.name)).distinct
    val fields = node.fields.map(_._1).zip(typeResults).zipWithIndex.map {
      case ((name, res), idx) => Proto.Field(ProtoNaming.snake(name), idx + 1, res.t)
    }
    val message = Proto.Message(node.name.parts.tail, Proto.MessageFields(fields))
    val source = Proto.Source(imports, message)

    val dir = node.name.tailName.initName.parts.foldLeft(out)(_.resolve(_))
    Files.createDirectories(dir)

    Using(Files.newBufferedWriter(dir.resolve(node.name.base + ".proto"), StandardCharsets.UTF_8)) { writer =>
      source.write(writer)
    }
  }
}
