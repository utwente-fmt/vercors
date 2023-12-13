package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{NodeDefinition, NodeGenerator, StructuralNode}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoNode extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit = {
    val typeResults = node.fields.map(_._2).map(ProtoNaming.getType)
    val indexOffset = if(node.kind == StructuralNode) 1 else 2
    val fields = node.fields.map(_._1).zip(typeResults).zipWithIndex.map {
      case ((name, res), idx) => Proto.Field(ProtoNaming.snake(name), idx + indexOffset, res.t)
    }
    val allFields =
      if(node.kind == StructuralNode) fields
      else Proto.Field("id", 1, Proto.Required(Proto.Long)) +: fields
    val name = ProtoNaming.getTypeName(node.name)
    val message = Proto.Message(name, Proto.MessageFields(allFields)).opaqueNodes
    val source = Proto.Source(
      Seq("scalapb", "scalapb") +: message.imports,
      Proto.renderOptions(Proto.OPAQUE_SUBMESSAGES_OPTIONS.updated("package_name", ProtoNaming.scalaPackageOption(name))),
      message,
    )

    val dir = message.name.init.foldLeft(out)(_.resolve(_))
    Files.createDirectories(dir)

    Using(Files.newBufferedWriter(dir.resolve(message.name.last + ".proto"), StandardCharsets.UTF_8)) { writer =>
      source.write(writer)
    }
  }
}
