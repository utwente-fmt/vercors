import ColDefs._
import MetaUtil.NonemptyMatch

import scala.meta._

case class ColHelperAbstractRewriter(info: ColDescription) {
  def rewriteDefaultCases(baseType: String): List[Case] = {
    val classes = info.defs.filter(cls => info.supports(baseType)(cls.baseName))

    classes.map(cls => Case(
      Pat.Typed(Pat.Var(q"node"), t"${cls.typ}[Pre]"),
      None,
      q"new ${cls.rewriteHelperName}(node).rewrite()"
    )).toList
  }

  def make(): List[Stat] = q"""
    import RewriteHelpers._
    abstract class AbstractRewriter[Pre, Post] extends $SCOPE_CONTEXT() {
      implicit val rewriter: AbstractRewriter[Pre, Post] = this

      def dispatch(decl: $DECLARATION_TYPE[Pre]): Unit

      def rewriteDefault(decl: $DECLARATION_TYPE[Pre]): Unit = ${
        NonemptyMatch("declaration rewriteDefault", q"decl", rewriteDefaultCases(DECLARATION))
      }.succeedDefault(decl)(this)

      ..${info.families.map(family => q"""
        def dispatch(node: ${Type.Name(family)}[Pre]): ${Type.Name(family)}[Post]
      """).toList}

      ..${info.families.map(family => q"""
        def rewriteDefault(node: ${Type.Name(family)}[Pre]): ${Type.Name(family)}[Post] = ${
          NonemptyMatch(s"$family rewriteDefault", q"node", rewriteDefaultCases(family))
        }
      """).toList}
    }
  """.stats
}
