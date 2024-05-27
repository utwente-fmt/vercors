package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming.{scopes, typ}
import vct.col.ast.structure
import vct.col.ast.structure.AllFamiliesGenerator

import java.nio.file.Path
import scala.meta._

class AbstractRewriter extends AllFamiliesGenerator {
  override def generate(
      out: Path,
      declaredFamilies: Seq[structure.Name],
      structuralFamilies: Seq[structure.Name],
  ): Unit =
    ResultStream.write(
      out.resolve("AbstractRewriter.scala"),
      getArw(declaredFamilies, structuralFamilies),
    )

  // TODO: anySucc should probably use the succProvider - how?
  def getArw(
      declaredFamilies: Seq[structure.Name],
      structuralFamilies: Seq[structure.Name],
  ): Source =
    source"""
      package vct.col.ast

      trait AbstractRewriter[Pre, Post] {
        implicit val rewriter: $AbstractRewriter[Pre, Post] = this

        def dispatch(o: $Origin): $Origin = o
        def dispatch[T <: $VerificationFailure](blame: $Blame[T]): $Blame[T] = blame

        def dispatch(decl: $Declaration[Pre]): $Unit

        @_root_.scala.deprecated("Use decl.rewriteDefault().succeed(decl) instead")
        def rewriteDefault(decl: $Declaration[Pre]): $Unit =
          allScopes.anySucceed(decl, decl.rewriteDefault())

        def porcelainRefSucc[RefDecl <: $Declaration[Post]](ref: $RefType[Pre, _])(implicit tag: $ClassTag[RefDecl]): $OptionType[$RefType[Post, RefDecl]] = None
        def porcelainRefSeqSucc[RefDecl <: $Declaration[Post]](refs: $SeqType[$RefType[Pre, _]])(implicit tag: $ClassTag[RefDecl]): $OptionType[$SeqType[$RefType[Post, RefDecl]]] = None

        val allScopes: $AllScopes[Pre, Post] = $AllScopesObj()
        def succProvider: $SuccessorsProvider[Pre, Post] = this.allScopes.freeze

        def anySucc[RefDecl <: $Declaration[Post]](`~decl`: $Declaration[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
          succProvider.anySucc[RefDecl](`~decl`)(tag)

        ..${declaredFamilies.map(succ).toList}
        ..${declaredFamilies.map(scope).toList}
        ..${structuralFamilies.map(dispatch).toList}
        ..${structuralFamilies.map(rewriteDefault).toList}
      }
    """

  def succ(family: structure.Name): Defn.Def =
    q"""
      def succ[RefDecl <: $Declaration[Post]](decl: ${typ(family)}[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
        this.succProvider.succ(decl)
    """

  def scope(family: structure.Name): Defn.Def =
    q"""
      def ${scopes(family.base)}: $Scopes[Pre, Post, ${typ(family)}[Pre], ${typ(family)}[Post]] =
        this.allScopes.${scopes(family.base)}
    """

  def dispatch(family: structure.Name): Decl.Def =
    q"""
      def dispatch(node: ${typ(family)}[Pre]): ${typ(family)}[Post]
    """

  def rewriteDefault(family: structure.Name): Defn.Def =
    q"""
      @_root_.scala.deprecated("Use node.rewriteDefault() instead")
      def rewriteDefault(node: ${typ(family)}[Pre]): ${typ(family)}[Post] =
        node.rewriteDefault()
    """
}
