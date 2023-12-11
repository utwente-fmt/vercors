package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{FamilyGenerator, Name, NodeKind}

import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoFamily extends FamilyGenerator {
  override def generate(out: Path, family: Name, kind: NodeKind, nodes: Seq[Name]): Unit = {
    if(nodes == Seq(family)) return

    val fqNames = nodes.map(_.tailName)

    val dir = family.tailName.initName.parts.foldLeft(out)(_.resolve(_))
    Files.createDirectories(dir)

    Using(Files.newBufferedWriter(dir.resolve(family.base + ".proto"))) { writer =>
      Proto.Source(
        imports = fqNames.map(_.parts),
        message = Proto.Message(family.parts.tail, Proto.MessageOneOf(
          oneOfName = "v",
          fields = fqNames.zipWithIndex.map {
            case (name, idx) => Proto.Field(ProtoNaming.snake(name.base), idx + 1, Proto.UnspecifiedArity(ProtoNaming.getType(name)))
          }
        ))
      ).write(writer)
    }
  }
}
