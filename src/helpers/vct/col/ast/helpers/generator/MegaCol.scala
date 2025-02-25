package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Proto
import vct.col.ast.structure.{AllNodesGenerator, NodeDefinition}

import java.nio.file.{Files, Path, Paths}
import scala.util.Using

class MegaCol extends AllNodesGenerator {
  override def generate(out: Path, definitions: Seq[NodeDefinition]): Unit = {
    val families = definitions.groupBy(_.family)

    val genNode = new ProtoNode()
    val genFamily = new ProtoFamily()
    val genAux = new ProtoAuxTypes()

    val messages =
      definitions.map(node => genNode.message(node)) ++
        families.flatMap(family =>
          genFamily.message(family._1, family._2.map(_.name)).toSeq
        ) ++ genAux.messages(definitions)

    Files.createDirectories(out.resolve(Paths.get("vct", "col", "ast")))
    Using(Files.newBufferedWriter(
      out.resolve(Paths.get("vct", "col", "ast", "col.proto"))
    )) { writer =>
      Proto.Source(
        messages.flatMap(_.namedTypes.collect { case t: Proto.StandardType =>
          t.fqName
        }).distinct,
        options = "",
        messages = messages,
      ).write(writer)
    }
  }
}
