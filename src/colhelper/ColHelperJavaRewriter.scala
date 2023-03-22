import ColDefs._
import ColHelperUtil.NonemptyMatch

import scala.meta._

case class ColHelperJavaRewriter(info: ColDescription) extends ColHelperMaker {
  def makeMapEntry(family: String)(cls: ClassDef): Term =
    q"""classOf[${cls.typ}[_]] -> new RWFunc[${Type.Name(family)}] {
      def apply[Pre, Post](node: ${Type.Name(family)}[Pre], rw: JavaRewriter[Pre, Post]): ${Type.Name(family)}[Post] =
        rw.${Term.Name("rewrite" + cls.baseName)}(node.asInstanceOf[${cls.typ}[Pre]])
    }"""

  def makeDeclMapEntry(cls: ClassDef): Term =
    q"""classOf[${cls.typ}[_]] -> new DeclRWFunc {
      def apply[Pre, Post](node: Declaration[Pre], rw: JavaRewriter[Pre, Post]): Unit =
        rw.${Term.Name("rewrite" + cls.baseName)}(node.asInstanceOf[${cls.typ}[Pre]])
    }"""

  def javaRewrite(ret: Type.Name)(cls: ClassDef): Stat = q"""
    def ${Term.Name("rewrite" + cls.baseName)}(node: ${cls.typ}[Pre]): $ret[Post] = {
      val builder = new ${cls.rewriteBuilderName}(node)
      ${Term.Name("build" + cls.baseName)}(builder)
      builder.build()
    }
  """

  def javaRewriteDecl(cls: ClassDef): Stat = q"""
    def ${Term.Name("rewrite" + cls.baseName)}(node: ${cls.typ}[Pre]): Unit = {
      val builder = new ${Init(cls.rewriteBuilderName, Name.Anonymous(), Nil)}[Pre, Post](node)
      ${Term.Name("build" + cls.baseName)}(builder)
      ${ColDefs.scopes(ColDefs.DECLARATION_KINDS.find(info.supports(_)(cls.baseName)).get)}
        .succeed(node, builder.build())
    }
  """

  def javaRewriteBuilder(cls: ClassDef): Stat =
    q"def ${Term.Name("build" + cls.baseName)}(builder: ${cls.rewriteBuilderName}[Pre, Post]): Unit = {}"

  def make(): List[(String, List[Stat])] = List("JavaRewriter" -> q"""
    object JavaRewriter {
      trait RWFunc[N[_] <: Node[_]] {
        def apply[Pre, Post](node: N[Pre], rw: JavaRewriter[Pre, Post]): N[Post]
      }

      trait DeclRWFunc {
        def apply[Pre, Post](node: Declaration[Pre], rw: JavaRewriter[Pre, Post]): Unit
      }

      ${Defn.Val(Nil,
        List(Pat.Var(Term.Name(s"rewriteDefaultDeclarationLookupTable"))),
        Some(t"Map[java.lang.Class[_], DeclRWFunc]"),
        q"Map(..${info.defs.filter(cls => info.supports("Declaration")(cls.baseName)).map(makeDeclMapEntry).toList})",
      )}

      ..${info.families.map(family => Defn.Val(Nil,
        List(Pat.Var(Term.Name(s"rewriteDefault${family}LookupTable"))),
        Some(t"Map[java.lang.Class[_], RWFunc[${Type.Name(family)}]]"),
        q"Map(..${info.defs.filter(cls => info.supports(family)(cls.baseName)).map(makeMapEntry(family)).toList})",
      )).toList}
    }

    abstract class JavaRewriter[Pre, Post] extends AbstractRewriter[Pre, Post] {
      def dispatch(decl: Declaration[Pre]): Unit =
        JavaRewriter.rewriteDefaultDeclarationLookupTable(decl.getClass)(decl, this)

      ..${
        val elems = info.defs.filter(cls => info.supports("Declaration")(cls.baseName))
        elems.map(javaRewriteDecl).toList ++ elems.map(javaRewriteBuilder).toList
      }

      ..${info.families.map(family => q"""
        def dispatch(node: ${Type.Name(family)}[Pre]): ${Type.Name(family)}[Post] =
          JavaRewriter.${Term.Name(s"rewriteDefault${family}LookupTable")}(node.getClass)(node, this)
      """).toList}

      ..${info.families.flatMap(family => {
        val elems = info.defs.filter(cls => info.supports(family)(cls.baseName))
        elems.map(javaRewrite(Type.Name(family))).toList ++ elems.map(javaRewriteBuilder).toList
      }).toList}
    }
  """.stats)
}
