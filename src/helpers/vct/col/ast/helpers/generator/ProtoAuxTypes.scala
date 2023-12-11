package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{AllNodesGenerator, NodeDefinition}

import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoAuxTypes extends AllNodesGenerator {
  override def generate(out: Path, definitions: Seq[NodeDefinition]): Unit = {
    val auxs =
      definitions
        .flatMap(_.fields)
        .map(_._2)
        .map(ProtoNaming.getType)
        .flatMap(_.auxs)
        .distinct
        .map(_.opaqueNodes)
        .sortBy(_.name.headOption)

    for(aux <- auxs) {
      val dir = aux.name.init.foldLeft(out)(_.resolve(_))
      Files.createDirectories(dir)

      Using(Files.newBufferedWriter(dir.resolve(aux.name.last + ".proto"))) { writer =>
        Proto.Source(
          Seq("scalapb", "scalapb") +: aux.imports,
          Proto.renderOptions(Proto.OPAQUE_SUBMESSAGES_OPTIONS),
          aux,
        ).write(writer)
      }
    }
  }
}
