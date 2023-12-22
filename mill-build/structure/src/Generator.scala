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
  def generate(out: Path, family: Name, kind: NodeKind, nodes: Seq[Name]): Unit
}

trait ImplTraitGenerator {
  def fix(p: Path, opsNames: Seq[String]): Unit
  def generate(p: Path, node: String, concrete: Boolean, family: Boolean): Unit
}