package vct.col.ast.structure.api

import java.nio.file.Path

trait AllFamiliesGeneratorApi {
  def generate(out: Path, declaredFamilies: String, structuralFamilies: String): Unit
}

trait AllNodesGeneratorApi {
  def generate(out: Path, definitions: String): Unit
}

trait NodeGeneratorApi {
  def generate(out: Path, node: String): Unit
}

trait FamilyGeneratorApi {
  def generate(out: Path, family: String, kind: String, nodes: String): Unit
}

trait ImplTraitGeneratorApi {
  def fix(p: Path, opsNames: String): Unit
  def generate(p: Path, node: String, concrete: Boolean, family: Boolean): Unit
}