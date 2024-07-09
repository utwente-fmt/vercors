package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming.{scopes, succProviderName, typ}
import vct.col.ast.structure
import vct.col.ast.structure.AllFamiliesGenerator

import java.nio.file.Path
import scala.meta._

class AllScopes extends AllFamiliesGenerator {
  override def generate(
      out: Path,
      declaredFamilies: Seq[structure.Name],
      structuralFamilies: Seq[structure.Name],
  ): Unit =
    ResultStream
      .write(out.resolve("AllScopes.scala"), allScopes(declaredFamilies))

  def allScopes(declaredFamilies: Seq[structure.Name]): Source =
    source"""
      package vct.col.ast

      case class AllScopes[Pre, Post]() extends ${Init(t"$SuccessorsProvider[Pre, Post]", Name.Anonymous(), List.empty)} {
        def freeze: $AllFrozenScopes[Pre, Post] = new $AllFrozenScopes(this)

        def anyDeclare[T <: $Declaration[Post]](`~decl`: T): T =
          ${Term.Match(q"`~decl`", declaredFamilies.map(name => Case(p"decl: ${typ(name)}[Post]", None, q"${scopes(name.base)}.declare(decl); `~decl`")).toList)}

        def anySucceedOnly[T <: $Declaration[Post]](`~pre`: $Declaration[Pre], `~post`: T)(implicit tag: $ClassTag[T]): T =
          ${Term.Match(
        q"(`~pre`, `~post`)",
        declaredFamilies.map(name =>
          Case(p"(pre: ${typ(name)}[Pre], post: ${typ(name)}[Post])", None, q"${scopes(name.base)}.succeedOnly(pre, post); `~post`")
        ).toList :+ Case(p"(pre, post)", None, q"throw $InconsistentSuccessionTypesObj(pre, post)"),
      )}

        def anySucceed[T <: $Declaration[Post]](`~pre`: $Declaration[Pre], `~post`: T)(implicit tag: $ClassTag[T]): T = {
          anyDeclare(`~post`)
          anySucceedOnly(`~pre`, `~post`)
        }

        ..${declaredFamilies.map(name => q"""
          val ${Pat.Var(scopes(name.base))}: $Scopes[Pre, Post, ${typ(name)}[Pre], ${typ(name)}[Post]] = $ScopesObj()
        """).toList}

        ..${declaredFamilies.map(name => q"""
          def ${succProviderName(name)}: $SuccessorProvider[Pre, Post, ${typ(name)}[Pre], ${typ(name)}[Post]] =
            this.${scopes(name.base)}.freeze
        """).toList}

        ..${declaredFamilies.map(name => q"""
          override def succ[RefDecl <: $Declaration[Post]](decl: ${typ(name)}[Pre])(implicit tag: $ClassTag[RefDecl]): $RefType[Post, RefDecl] =
            this.${scopes(name.base)}.freeze.succ(decl)
        """).toList}
      }
    """
}
