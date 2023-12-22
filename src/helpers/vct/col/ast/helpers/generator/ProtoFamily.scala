package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{FamilyGenerator, Name, NodeKind}

import java.nio.file.{Files, Path}
import scala.util.Using

class ProtoFamily extends FamilyGenerator {
  override def generate(out: Path, family: Name, kind: NodeKind, nodes: Seq[Name]): Unit = {
    if(nodes == Seq(family)) return

    val fqNames = nodes.map(ProtoNaming.getTypeName)

    val name = ProtoNaming.getTypeName(family)
    val dir = name.init.foldLeft(out)(_.resolve(_))
    Files.createDirectories(dir)

    Using(Files.newBufferedWriter(dir.resolve(family.base + ".proto"))) { writer =>
      Proto.Source(
        imports = Seq("scalapb", "scalapb") +: fqNames,
        options = Proto.renderOptions(Proto.STANDARD_OPTIONS.updated("package_name", ProtoNaming.scalaPackageOption(name))),
        message = Proto.Message(name, Proto.MessageOneOf(
          oneOfName = "v",
          fields = fqNames.zipWithIndex.map {
            case (name, idx) => Proto.Field(
              name = ProtoNaming.snake(name.last),
              index = idx + 1,
              t = Proto.UnspecifiedArity(Proto.FamilyType(name))
            )
          }
        ))
      ).write(writer)
    }
  }
}
