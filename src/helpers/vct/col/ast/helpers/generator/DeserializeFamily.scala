package vct.col.ast.helpers.generator

import vct.col.ast.structure.{FamilyGenerator, Name, NodeKind}

import java.nio.file.Path

class DeserializeFamily extends FamilyGenerator {
  override def generate(out: Path, family: Name, kind: NodeKind, nodes: Seq[Name]): Unit = {}
}
