package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants.AbstractRewriter
import vct.col.ast.helpers.defn.Naming.typ
import vct.col.ast.structure
import vct.col.ast.structure.AllFamiliesGenerator

import java.nio.file.Path
import scala.meta._

class BaseCoercingRewriter extends AllFamiliesGenerator {
  override def generate(out: Path, declaredFamilies: Seq[structure.Name], structuralFamilies: Seq[structure.Name]): Unit =
    ResultStream.write(out.resolve("BaseCoercingRewriter.scala"), crw(structuralFamilies))

  def crw(families: Seq[structure.Name]): Source =
    source"""
      package vct.col.ast.rewrite

      trait BaseCoercingRewriter[Pre, Post] extends ${Init(t"$AbstractRewriter[Pre, Post]", Name.Anonymous(), List.empty)} {
        ..${families.map(dispatch).toList}
        ..${families.map(coerce).toList}
        ..${families.map(preCoerce).toList}
        ..${families.map(postCoerce).toList}
      }
    """

  def dispatch(family: structure.Name): Defn.Def =
    q"""
      override final def dispatch(node: ${typ(family)}[Pre]): ${typ(family)}[Post] =
        postCoerce(coerce(preCoerce(node)))
    """

  def coerce(family: structure.Name): Decl.Def =
    q"""
      def coerce(node: ${typ(family)}[Pre]): ${typ(family)}[Pre]
    """

  def preCoerce(family: structure.Name): Defn.Def =
    q"""
      def preCoerce(node: ${typ(family)}[Pre]): ${typ(family)}[Pre] =
        node
    """

  def postCoerce(family: structure.Name): Defn.Def =
    q"""
      def postCoerce(node: ${typ(family)}[Pre]): ${typ(family)}[Post] =
        node.rewriteDefault()
    """
}
