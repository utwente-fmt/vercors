package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{AllNodesGenerator, NodeDefinition}

import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoAuxTypes extends AllNodesGenerator {
  def messages(definitions: Seq[NodeDefinition]): Seq[Proto.Message] =
    definitions.flatMap(_.fields).map(_._2).map(ProtoNaming.getType)
      .flatMap(_.auxs).distinct.sortBy(_.name.headOption)

  override def generate(out: Path, definitions: Seq[NodeDefinition]): Unit = {
    val auxs = messages(definitions).map(_.opaqueNodes)

    for (aux <- auxs) {
      val dir = aux.name.init.foldLeft(out)(_.resolve(_))
      Files.createDirectories(dir)

      Using(Files.newBufferedWriter(dir.resolve(aux.name.last + ".proto"))) {
        writer =>
          Proto.Source(
            Seq("scalapb", "scalapb") +: aux.imports,
            Proto.renderOptions(Proto.OPAQUE_SUBMESSAGES_OPTIONS.updated(
              "package_name",
              ProtoNaming.scalaPackageOption(aux.name),
            )),
            aux,
          ).write(writer)
      }
    }
  }
}
