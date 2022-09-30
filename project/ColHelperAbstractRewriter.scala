import ColDefs._
import MetaUtil.NonemptyMatch

import scala.meta._

case class ColHelperAbstractRewriter(info: ColDescription) {
  def rewriteDefaultCases(baseType: String): List[Term] = {
    val classes = info.defs.filter(cls => info.supports(baseType)(cls.baseName))

    classes.map { cls => q"""
      classOf[${cls.typ}[_]] -> new RWFunc[${Type.Name(baseType)}] {
        def apply[Pre, Post](node: ${Type.Name(baseType)}[Pre], rw: AbstractRewriter[Pre, Post]): ${Type.Name(baseType)}[Post] =
          ${ColDefs.DECLARATION_KINDS.find(info.supports(_)(cls.baseName)) match {
            case None => q"new ${cls.rewriteHelperName}(node.asInstanceOf[${cls.typ}[Pre]])(rw).rewrite()"
            case Some(decl) => q"rw.${ColDefs.scopes(decl)}.succeed(node.asInstanceOf[${cls.typ}[Pre]], new ${cls.rewriteHelperName}(node.asInstanceOf[${cls.typ}[Pre]])(rw).rewrite())"
          }}
      }
    """}.toList
  }

  def make(): List[Stat] = q"""
    import scala.reflect.ClassTag
    import RewriteHelpers._
    import vct.col.util.Scopes
    import vct.col.ref.LazyRef

    object AbstractRewriter {
      trait RWFunc[N[_] <: Node[_]] {
        def apply[Pre, Post](node: N[Pre], rw: AbstractRewriter[Pre, Post]): N[Post]
      }

      ${Defn.Val(Nil,
        List(Pat.Var(Term.Name(s"rewriteDefaultDeclarationLookupTable"))),
        Some(t"Map[java.lang.Class[_], RWFunc[Declaration]]"),
        q"Map(..${rewriteDefaultCases("Declaration")})",
      )}

      ..${info.families.map(family => Defn.Val(Nil,
        List(Pat.Var(Term.Name(s"rewriteDefault${family}LookupTable"))),
        Some(t"Map[java.lang.Class[_], RWFunc[${Type.Name(family)}]]"),
        q"Map(..${rewriteDefaultCases(family)})",
      )).toList}
    }

    abstract class AbstractRewriter[Pre, Post] {
      implicit val rewriter: AbstractRewriter[Pre, Post] = this

      def dispatch(o: Origin): Origin = o

      def dispatch(decl: Declaration[Pre]): Unit

      def rewriteDefault(decl: Declaration[Pre]): Unit =
        AbstractRewriter.${Term.Name(s"rewriteDefaultDeclarationLookupTable")}(decl.getClass)(decl, this)

      def porcelainRefSucc[RefDecl <: Declaration[Post]](ref: Ref[Pre, _])(implicit tag: ClassTag[RefDecl]): Option[Ref[Post, RefDecl]] = None
      def porcelainRefSeqSucc[RefDecl <: Declaration[Post]](refs: Seq[Ref[Pre, _]])(implicit tag: ClassTag[RefDecl]): Option[Seq[Ref[Post, RefDecl]]] = None

      val allScopes: AllScopes[Pre, Post] = AllScopes()
      def succProvider: SuccessorsProvider[Pre, Post] = allScopes.freeze

      def anySucc[RefDecl <: Declaration[Post]](decl: Declaration[Pre])(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
        ${MetaUtil.NonemptyMatch("decl succ kind cases", q"decl", ColDefs.DECLARATION_KINDS.map(decl =>
          Case(p"decl: ${Type.Name(decl)}[Pre]", None, q"succ(decl)")
        ).toList)}

      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def ${ColDefs.scopes(decl)}: Scopes[Pre, Post, ${Type.Name(decl)}[Pre], ${Type.Name(decl)}[Post]] = allScopes.${ColDefs.scopes(decl)}
      """).toList}

      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def succ[RefDecl <: Declaration[Post]](decl: ${Type.Name(decl)}[Pre])(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
          succProvider.succ(decl)
      """).toList}

      ..${info.families.map(family => q"""
        def dispatch(node: ${Type.Name(family)}[Pre]): ${Type.Name(family)}[Post]
      """).toList}

      ..${info.families.flatMap(family => Seq(q"""
        def rewriteDefault(node: ${Type.Name(family)}[Pre]): ${Type.Name(family)}[Post] =
          AbstractRewriter.${Term.Name(s"rewriteDefault${family}LookupTable")}(node.getClass)(node, this)
      """)).toList}
    }
  """.stats
}
