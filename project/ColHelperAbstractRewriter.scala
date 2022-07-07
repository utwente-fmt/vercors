import ColDefs._
import MetaUtil.NonemptyMatch

import scala.meta._

case class ColHelperAbstractRewriter(info: ColDescription) {
  def rewriteDefaultCases(baseType: String): List[Term] = {
    val classes = info.defs.filter(cls => info.supports(baseType)(cls.baseName))

    classes.map { cls => q"""
      classOf[${cls.typ}[_]] -> new RWFunc[${Type.Name(baseType)}] {
        def apply[Pre, Post](node: ${Type.Name(baseType)}[Pre], rw: AbstractRewriter[Pre, Post]): ${Type.Name(baseType)}[Post] =
          new ${cls.rewriteHelperName}(node.asInstanceOf[${cls.typ}[Pre]])(rw).rewrite()
      }
    """}.toList
  }

  def make(): List[Stat] = q"""
    import scala.reflect.ClassTag
    import RewriteHelpers._
    import vct.col.util.Scopes

    object AbstractRewriter {
      trait RWFunc[N[_] <: Node[_]] {
        def apply[Pre, Post](node: N[Pre], rw: AbstractRewriter[Pre, Post]): N[Post]
      }

      ${Defn.Val(Nil,
        List(Pat.Var(Term.Name(s"rewriteDefault${DECLARATION}LookupTable"))),
        Some(t"Map[java.lang.Class[_], RWFunc[$DECLARATION_TYPE]]"),
        q"Map(..${rewriteDefaultCases(DECLARATION)})",
      )}

      ..${info.families.map(family => Defn.Val(Nil,
        List(Pat.Var(Term.Name(s"rewriteDefault${family}LookupTable"))),
        Some(t"Map[java.lang.Class[_], RWFunc[${Type.Name(family)}]]"),
        q"Map(..${rewriteDefaultCases(family)})",
      )).toList}
    }

    abstract class AbstractRewriter[Pre, Post] extends $SCOPE_CONTEXT() {
      implicit val rewriter: AbstractRewriter[Pre, Post] = this

      def dispatch(o: Origin): Origin = o

      def dispatch(decl: $DECLARATION_TYPE[Pre]): Unit

      def rewriteDefault(decl: $DECLARATION_TYPE[Pre]): Unit =
        AbstractRewriter.${Term.Name(s"rewriteDefault${DECLARATION}LookupTable")}(decl.getClass)(decl, this)
          .succeedDefault(decl)(this)

      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        val ${Pat.Var(ColDefs.scopes(decl))}: Scopes[Pre, Post, ${Type.Name(decl)}[Pre], ${Type.Name(decl)}[Post]] = Scopes(this)
      """).toList}

      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def succ[RefDecl <: ${Type.Name(decl)}[Post]](decl: ${Type.Name(decl)}[Pre])(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
          ${ColDefs.scopes(decl)}.freeze.succ(decl)
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
