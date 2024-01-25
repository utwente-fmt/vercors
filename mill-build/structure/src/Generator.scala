package vct.col.ast.structure

import vct.col.ast.structure.api._

import java.nio.file.Path
import upickle.default.read

trait AllFamiliesGenerator extends AllFamiliesGeneratorApi {
  override def generate(out: Path, declaredFamilies: String, structuralFamilies: String): Unit =
    generate(out, read[Seq[Name]](declaredFamilies), read[Seq[Name]](structuralFamilies))
  def generate(out: Path, declaredFamilies: Seq[Name], structuralFamilies: Seq[Name]): Unit
}

trait AllNodesGenerator extends AllNodesGeneratorApi {
  override def generate(out: Path, definitions: String): Unit =
    generate(out, read[Seq[NodeDefinition]](definitions))
  def generate(out: Path, definitions: Seq[NodeDefinition]): Unit
}

trait NodeGenerator extends NodeGeneratorApi {
  override def generate(out: Path, node: String): Unit =
    generate(out, read[NodeDefinition](node))
  def generate(out: Path, node: NodeDefinition): Unit
}

trait FamilyGenerator extends FamilyGeneratorApi {
  override def generate(out: Path, family: String, kind: String, nodes: String): Unit =
    generate(out, read[Name](family), read[NodeKind](kind), read[Seq[Name]](nodes))
  def generate(out: Path, family: Name, kind: NodeKind, nodes: Seq[Name]): Unit
}

trait ImplTraitGenerator extends ImplTraitGeneratorApi {
  override def fix(p: Path, opsNames: String): Unit =
    fix(p, read[Seq[String]](opsNames))
  def fix(p: Path, opsNames: Seq[String]): Unit
  def generate(p: Path, node: String, concrete: Boolean, family: Boolean): Unit
}