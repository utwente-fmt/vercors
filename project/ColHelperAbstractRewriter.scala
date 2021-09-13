import ColDefs._
import MetaUtil.NonemptyMatch

import scala.meta._

case class ColHelperAbstractRewriter(info: ColDescription) {
  def rewriteDefaultCases(baseType: String): List[Case] = {
    val classes = info.defs.filter(cls => info.supports(baseType)(cls.baseName))

    classes.map(cls => Case(
      Pat.Typed(Pat.Var(q"node"), cls.typ),
      None,
      q"node.rewrite()"
    )).toList
  }

  def make(): List[Stat] = q"""
    import RewriteHelpers._
    abstract class AbstractRewriter extends $SCOPE_CONTEXT() {
      implicit val rewriter: AbstractRewriter = this

      def dispatch(decl: $DECLARATION_TYPE): Unit

      def rewriteDefault(decl: $DECLARATION_TYPE): Unit = ${
        NonemptyMatch("declaration rewriteDefault", q"decl", rewriteDefaultCases(DECLARATION))
      }.succeedDefault(this, decl)

      ..${info.families.map(family => q"""
        def dispatch(node: ${Type.Name(family)}): ${Type.Name(family)}
      """).toList}

      ..${info.families.map(family => q"""
        def rewriteDefault(node: ${Type.Name(family)}): ${Type.Name(family)} = ${
          NonemptyMatch(s"$family rewriteDefault", q"node", rewriteDefaultCases(family))
        }
      """).toList}
    }
  """.stats
}
