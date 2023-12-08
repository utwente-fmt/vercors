package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.ProtoNaming.ucamel
import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{FamilyGenerator, Name, NodeKind}

import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoFamily extends FamilyGenerator {
  override def generate(out: Path, family: Name, kind: NodeKind, nodes: Seq[Name]): Unit = {
    if(nodes == Seq(family)) return

    val fqNames = nodes.map(_.tailName)

    Using(Files.newBufferedWriter(out.resolve(family.base + ".protomessage"))) { writer =>
      Proto.Message(Seq(ucamel(family.base)), Proto.MessageOneOf(
        oneOfName = "v",
        fields = fqNames.zipWithIndex.map {
          case (name, idx) => Proto.Field(ProtoNaming.snake(name.base), idx + 1, Proto.UnspecifiedArity(ProtoNaming.getType(name)))
        }
      )).write(writer)
    }
  }
}
