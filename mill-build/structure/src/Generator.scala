package vct.col.ast.structure

import java.nio.file.Path

trait AllFamiliesGenerator {
  def generate(out: Path, declaredFamilies: Seq[Name], structuralFamilies: Seq[Name]): Unit
}

trait AllNodesGenerator {
  def generate(out: Path, definitions: Seq[NodeDefinition]): Unit
}

trait NodeGenerator {
  def generate(out: Path, node: NodeDefinition): Unit
}

trait FamilyGenerator {
  def generate(out: Path, family: Name, nodes: Seq[Name]): Unit
}

