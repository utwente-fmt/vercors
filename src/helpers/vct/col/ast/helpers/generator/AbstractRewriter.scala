package vct.col.ast.helpers.generator

import vct.col.ast.structure
import vct.col.ast.structure.AllFamiliesGenerator

import java.nio.file.Path
import scala.meta._

class AbstractRewriter extends AllFamiliesGenerator {
  override def generate(out: Path, declaredFamilies: Seq[structure.Name], structuralFamilies: Seq[structure.Name]): Unit =
    ResultStream.write(out.resolve("AbstractRewriter.scala"), getArw(declaredFamilies, structuralFamilies))

  def getArw(declaredFamilies: Seq[structure.Name], structuralFamilies: Seq[structure.Name]): Source =
    source"""
      package vct.col.ast

      trait AbstractRewriter[Pre, Post]
    """
}
