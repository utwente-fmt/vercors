package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Naming.{succProviderName, typ}
import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.structure
import vct.col.ast.structure.AllFamiliesGenerator
import vct.col.ast.structure.Type.Nothing

import java.nio.file.Path
import scala.meta._

class SuccessorsProvider extends AllFamiliesGenerator {
  override def generate(
      out: Path,
      declaredFamilies: Seq[structure.Name],
      structuralFamilies: Seq[structure.Name],
  ): Unit =
    ResultStream
      .write(out.resolve("SuccessorsProvider.scala"), source(declaredFamilies))

  def source(declaredFamilies: Seq[structure.Name]): Source =
    source"""
      package vct.col.ast

      ${successorsProvider(declaredFamilies)}
      ${successorsProviderNothing(declaredFamilies)}
      ${successorsProviderChain(declaredFamilies)}
      ${successorsProviderTrafo(declaredFamilies)}
    """

  def successorsProvider(declaredFamilies: Seq[structure.Name]): Defn.Trait =
    q"""
      trait SuccessorsProvider[Pre, Post] {
        def anySucc[RefDecl <: $Declaration[Post]](`~decl`: $Declaration[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
          ${Term.Match(q"`~decl`", declaredFamilies.map(name => Case(p"(decl: ${typ(name)}[Pre])", None, q"succ(decl)")).toList)}

        ..${declaredFamilies.map(name => q"""
          def ${succProviderName(name)}: $SuccessorProvider[Pre, Post, ${typ(name)}[Pre], ${typ(name)}[Post]]
        """).toList}

        ..${declaredFamilies.map(name => q"""
          def succ[RefDecl <: $Declaration[Post]](decl: ${typ(name)}[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
            ${succProviderName(name)}.succ(decl)(tag)
        """).toList}
      }
    """

  def successorsProviderNothing(
      declaredFamilies: Seq[structure.Name]
  ): Defn.Class =
    q"""
      class SuccessorsProviderNothing[Pre, Post](crash: => Nothing) extends ${Init(t"$SuccessorsProvider[Pre, Post]", Name.Anonymous(), List.empty)} {
        ..${declaredFamilies.map(name => q"""
          override def ${succProviderName(name)}: $SuccessorProvider[Pre, Post, ${typ(name)}[Pre], ${typ(name)}[Post]] =
            new $SuccessorProviderNothing(crash)
        """).toList}
      }
    """

  def successorsProviderChain(
      declaredFamilies: Seq[structure.Name]
  ): Defn.Class =
    q"""
      case class SuccessorsProviderChain[A, B, C](left: $SuccessorsProvider[A, B], right: $SuccessorsProvider[B, C]) extends ${Init(t"SuccessorsProvider[A, C]", Name.Anonymous(), List.empty)} {
        ..${declaredFamilies.map(name => q"""
          override def ${succProviderName(name)}: $SuccessorProvider[A, C, ${typ(name)}[A], ${typ(name)}[C]] =
            new $SuccessorProviderChain(left.${succProviderName(name)}, right.${succProviderName(name)})
        """).toList}
      }
    """

  def successorsProviderTrafo(
      declaredFamilies: Seq[structure.Name]
  ): Defn.Class =
    q"""
      abstract class SuccessorsProviderTrafo[Pre, Post](inner: $SuccessorsProvider[Pre, Post]) extends ${Init(t"$SuccessorsProvider[Pre, Post]", Name.Anonymous(), List.empty)} { outer =>
        def preTransform[I <: $Declaration[Pre], O <: $Declaration[Post]](pre: I): $OptionType[O] = None
        def postTransform[T <: $Declaration[Post]](pre: $Declaration[Pre], post: $OptionType[T]): $OptionType[T] = post

        ..${declaredFamilies.map((name: structure.Name) => q"""
          override def ${succProviderName(name)}: $SuccessorProvider[Pre, Post, ${typ(name)}[Pre], ${typ(name)}[Post]] =
            new ${Init(t"$SuccessorProviderTrafo[Pre, Post, ${typ(name)}[Pre], ${typ(name)}[Post]]", Name.Anonymous(), List(List(q"inner.${succProviderName(name)}")))} {
              override def preTransform[I <: ${typ(name)}[Pre], O <: ${typ(name)}[Post]](pre: I): $OptionType[O] =
                outer.preTransform[I, O](pre)

              override def postTransform[T <: ${typ(name)}[Post]](pre: ${typ(name)}[Pre], post: $OptionType[T]): $OptionType[T] =
                outer.postTransform[T](pre, post)
            }
        """).toList}
      }
    """
}
