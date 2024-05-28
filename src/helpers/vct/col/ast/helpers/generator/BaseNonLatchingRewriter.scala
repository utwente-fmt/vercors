package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming.typ
import vct.col.ast.structure
import vct.col.ast.structure.AllFamiliesGenerator

import java.nio.file.Path
import scala.meta._

class BaseNonLatchingRewriter extends AllFamiliesGenerator {
  override def generate(
      out: Path,
      declaredFamilies: Seq[structure.Name],
      structuralFamilies: Seq[structure.Name],
  ): Unit =
    ResultStream.write(
      out.resolve("BaseNonLatchingRewriter.scala"),
      rw(declaredFamilies, structuralFamilies),
    )

  def rw(
      declaredFamilies: Seq[structure.Name],
      structuralFamilies: Seq[structure.Name],
  ): Source =
    source"""
      package vct.col.ast.rewrite

      trait BaseNonLatchingRewriter[Pre, Post] extends ${Init(t"$AbstractRewriter[Pre, Post]", Name.Anonymous(), List.empty)} {
        ..${structuralFamilies.map(dispatch).toList}
      }
    """

  def dispatch(family: structure.Name): Defn.Def =
    q"""
      def dispatch(node: ${typ(family)}[Pre]): ${typ(family)}[Post] =
        node.rewriteDefault()
    """
}
