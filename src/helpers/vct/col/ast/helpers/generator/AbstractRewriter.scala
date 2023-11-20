package vct.col.ast.helpers.generator

import vct.col.ast.structure.{AllFamiliesGenerator, Name}

import java.nio.file.Path

class AbstractRewriter extends AllFamiliesGenerator {
  override def generate(out: Path, families: Seq[Name]): Unit = ???
}
