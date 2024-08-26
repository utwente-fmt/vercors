package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming.{scopes, typ}
import vct.col.ast.structure
import vct.col.ast.structure.AllFamiliesGenerator

import java.nio.file.Path
import scala.meta._

class AllFrozenScopes extends AllFamiliesGenerator {
  override def generate(
      out: Path,
      declaredFamilies: Seq[structure.Name],
      structuralFamilies: Seq[structure.Name],
  ): Unit =
    ResultStream.write(
      out.resolve("AllFrozenScopes.scala"),
      allFrozenScopes(declaredFamilies),
    )

  def allFrozenScopes(declaredFamilies: Seq[structure.Name]): Source =
    source"""
      package vct.col.ast

      class AllFrozenScopes[Pre, Post](`~scopes`: $AllScopes[Pre, Post]) {
        override def equals(`~obj`: $Any): $Boolean = `~obj` match {
          case other: $AllFrozenScopes[_, _] =>
            ${declaredFamilies
        .map(name => q"this.${scopes(name.base)} == other.${scopes(name.base)}")
        .reduce[Term] { case (l, r) => q"$l && $r" }}
          case _ => false
        }

        ..${declaredFamilies.map(name => q"""
          val ${Pat.Var(scopes(name.base))} = `~scopes`.${scopes(name.base)}.freeze
        """).toList}

        ..${declaredFamilies.map(name => q"""
          def computeSucc(decl: ${typ(name)}[Pre]): $OptionType[${typ(name)}[Post]] =
            ${scopes(name.base)}.computeSucc(decl)
        """).toList}

        ..${declaredFamilies.map(name => q"""
          def succ[RefDecl <: $Declaration[Post]](decl: ${typ(name)}[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
            ${scopes(name.base)}.succ(decl)
        """).toList}
      }
    """
}
