package vct.col.ast.helpers.generator

import vct.col.ast.structure.{FamilyGenerator, Name}

import java.nio.file.Path

class ProtoFamily extends FamilyGenerator {
  override def generate(out: Path, family: Name, nodes: Seq[Name]): Unit = ???
}
