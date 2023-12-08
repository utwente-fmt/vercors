package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.ProtoNaming
import vct.col.ast.structure.{AllNodesGenerator, NodeDefinition}

import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoAuxTypes extends AllNodesGenerator {
  override def generate(out: Path, definitions: Seq[NodeDefinition]): Unit = {
    val auxs = definitions.flatMap(_.fields.flatMap(field => ProtoNaming.getType(field._2).auxs)).distinct.sortBy(_.name.lastOption)

    Using(Files.newBufferedWriter(out.resolve("aux.protomessage"))) { writer =>
      for(aux <- auxs) {
        aux.write(writer)
        writer.append("\n")
      }
    }
  }
}
