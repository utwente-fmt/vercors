package vct.col.ast.helpers.generator

import vct.col.ast.structure
import vct.col.ast.structure.{AllFamiliesGenerator, Name}

import java.nio.file.Path

class NonLatchingRewriter extends AllFamiliesGenerator {
  override def generate(out: Path, declaredFamilies: Seq[structure.Name], structuralFamilies: Seq[structure.Name]): Unit = {}
}
