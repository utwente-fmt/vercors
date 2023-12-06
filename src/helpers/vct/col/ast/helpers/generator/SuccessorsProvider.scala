package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Naming.typ
import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.structure
import vct.col.ast.structure.{AllFamiliesGenerator}

import java.nio.file.Path
import scala.meta._

class SuccessorsProvider extends AllFamiliesGenerator {
  override def generate(out: Path, declaredFamilies: Seq[structure.Name], structuralFamilies: Seq[structure.Name]): Unit =
    ResultStream.write(out.resolve("SuccessorsProvider.scala"), source(declaredFamilies))

  def source(declaredFamilies: Seq[structure.Name]): Source =
    source"""
      package vct.col.ast

      ${successorsProvider(declaredFamilies)}
      ${successorsProviderChain(declaredFamilies)}
      ${successorsProviderTrafo(declaredFamilies)}
    """

  def successorsProvider(declaredFamilies: Seq[structure.Name]): Defn.Trait =
    q"""
      trait SuccessorsProvider[Pre, Post] {
        def anySucc[RefDecl <: $Declaration[Post]](`~decl`: $Declaration[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
          ${Term.Match(q"`~decl`", declaredFamilies.map(name =>
            Case(p"(decl: ${typ(name)}[Pre])", None, q"succ(decl)")
          ).toList)}

        ..${declaredFamilies.map(name => q"""
          def computeSucc(decl: ${typ(name)}[Pre]): $OptionType[${typ(name)}[Post]]
        """).toList}

        ..${declaredFamilies.map(name => q"""
          def succ[RefDecl <: $Declaration[Post]](decl: ${typ(name)}[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
            new ${Init(t"$LazyRef[Post, RefDecl]", Name.Anonymous(), List(List(q"this.computeSucc(decl).get")))}
        """).toList}
      }
    """

  def successorsProviderChain(declaredFamilies: Seq[structure.Name]): Defn.Class =
    q"""
      case class SuccessorsProviderChain[A, B, C](left: $SuccessorsProvider[A, B], right: $SuccessorsProvider[B, C]) extends ${Init(t"SuccessorsProvider[A, C]", Name.Anonymous(), List.empty)} {
        ..${declaredFamilies.map(name => q"""
          def computeSucc(decl: ${typ(name)}[A]): $OptionType[${typ(name)}[C]] =
            this.left.computeSucc(decl).flatMap(this.right.computeSucc)
        """).toList}
      }
    """

  def successorsProviderTrafo(declaredFamilies: Seq[structure.Name]): Defn.Class =
    q"""
      abstract class SuccessorsProviderTrafo[Pre, Post](inner: $SuccessorsProvider[Pre, Post]) extends ${Init(t"$SuccessorsProvider[Pre, Post]", Name.Anonymous(), List.empty)} {
        def preTransform[I <: $Declaration[Pre], O <: $Declaration[Post]](pre: I): $OptionType[O] = None
        def postTransform[T <: $Declaration[Post]](pre: $Declaration[Pre], post: $OptionType[T]): $OptionType[T] = post

        ..${declaredFamilies.map((name: structure.Name) => q"""
          def computeSucc(decl: ${typ(name)}[Pre]): $OptionType[${typ(name)}[Post]] =
            this.preTransform(decl).orElse(this.postTransform(decl, this.inner.computeSucc(decl)))
        """).toList}
      }
    """
}
