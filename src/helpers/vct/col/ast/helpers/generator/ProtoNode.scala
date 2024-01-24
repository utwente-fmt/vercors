package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{DeclaredNode, NodeDefinition, NodeGenerator, StructuralNode}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoNode extends NodeGenerator {
  def message(node: NodeDefinition): Proto.Message = {
    val typeResults = node.fields.map(_._2).map(ProtoNaming.getType)
    val id = "id" -> Proto.Required(Proto.Long)
    val fields = node.fields.map(_._1).zip(typeResults).map {
      case (name, res) => ProtoNaming.snake(name) -> res.t
    }
    val blame = "blame" -> Proto.Required(Proto.StandardType(Proto.auxBase :+ "Blame"))
    val origin = "origin" -> Proto.Required(Proto.StandardType(Proto.auxBase :+ "Origin"))

    val allFieldDecls =
      (if(node.kind == DeclaredNode) Seq(id) else Nil) ++
        fields ++
        (if(node.blameType.isDefined) Seq(blame) else Nil) ++
        Seq(origin)

    val allFields = allFieldDecls.zipWithIndex.map {
      case (name -> t, idx) => Proto.Field(name, idx+1, t)
    }

    val name = ProtoNaming.getTypeName(node.name)
    Proto.Message(name, Proto.MessageFields(allFields))
  }

  override def generate(out: Path, node: NodeDefinition): Unit = {
    val message = this.message(node).opaqueNodes

    val source = Proto.Source(
      Seq("scalapb", "scalapb") +: message.imports,
      Proto.renderOptions(Proto.OPAQUE_SUBMESSAGES_OPTIONS.updated("package_name", ProtoNaming.scalaPackageOption(message.name))),
      message,
    )

    val dir = message.name.init.foldLeft(out)(_.resolve(_))
    Files.createDirectories(dir)

    Using(Files.newBufferedWriter(dir.resolve(message.name.last + ".proto"), StandardCharsets.UTF_8)) { writer =>
      source.write(writer)
    }
  }
}
