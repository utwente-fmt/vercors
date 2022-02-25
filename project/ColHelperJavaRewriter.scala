import ColDefs._
import MetaUtil.NonemptyMatch

import scala.meta._

case class ColHelperJavaRewriter(info: ColDescription) {
  def dispatchCase(cls: ClassDef): Case =
    Case(Pat.Typed(Pat.Var(q"node"), t"${cls.typ}[Pre]"), None, q"rewrite(node)")

  def javaRewrite(ret: Type.Name)(cls: ClassDef): Stat = q"""
    def rewrite(node: ${cls.typ}[Pre]): $ret[Post] = {
      val builder = new RewriteBuilders.${cls.rewriteBuilderName}(node)
      rewrite(builder)
      builder.build()
    }
  """

  def javaRewriteDecl(cls: ClassDef): Stat = q"""
    def rewrite(node: ${cls.typ}[Pre]): Unit = {
      val builder = new RewriteBuilders.${cls.rewriteBuilderName}[Pre, Post](node)
      rewrite(builder)
      builder.build().succeedDefault(node)(this)
    }
  """

  def javaRewriteBuilder(cls: ClassDef): Stat =
    q"def rewrite(builder: RewriteBuilders.${cls.rewriteBuilderName}[Pre, Post]): Unit = {}"

  def make(): List[Stat] = List(q"""
    abstract class JavaRewriter[Pre, Post] extends AbstractRewriter[Pre, Post] {
      def dispatch(decl: $DECLARATION_TYPE[Pre]): Unit =
        ${NonemptyMatch("declaration dispatch", q"decl", info.defs.filter(cls => info.supports(DECLARATION)(cls.baseName)).map(dispatchCase).toList)}

      ..${
        val elems = info.defs.filter(cls => info.supports(DECLARATION)(cls.baseName))
        elems.map(javaRewriteDecl).toList ++ elems.map(javaRewriteBuilder).toList
      }

      ..${info.families.map(family => q"""
        def dispatch(node: ${Type.Name(family)}[Pre]): ${Type.Name(family)}[Post] =
          ${NonemptyMatch(s"$family dispatch", q"node", info.defs.filter(cls => info.supports(family)(cls.baseName)).map(dispatchCase).toList)}
      """).toList}

      ..${info.families.flatMap(family => {
        val elems = info.defs.filter(cls => info.supports(family)(cls.baseName))
        elems.map(javaRewrite(Type.Name(family))).toList ++ elems.map(javaRewriteBuilder).toList
      }).toList}
    }
  """)
}
