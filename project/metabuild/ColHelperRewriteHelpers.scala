import ColDefs._

import scala.meta._

case class ColHelperRewriteHelpers(info: ColDescription) extends ColHelperMaker {
  def rewriteHelperParam(param: Term.Param): Term.Param =
    Term.Param(List(),
      param.name,
      Some(Type.ByName(ColHelperUtil.substituteTypeName("G", Type.Name("Post"))(param.decltpe.get))),
      Some(info.rewriteDefault(Term.Select(q"subject", Term.Name(param.name.value)), param.decltpe.get)))

  def makeRewriteHelperImpl(cls: ClassDef): (String, List[Stat]) = ("Rewrite" + cls.baseName + "Impl") -> List(q"""
    trait ${Type.Name("Rewrite" + cls.baseName + "Impl")}[Pre, Post] { this: RewriteHelpers.${cls.rewriteHelperName}[Pre, Post] =>
      def rewriteDefault(): ${cls.typ}[Post] = rewrite()

      def rewrite(..${cls.params.map(rewriteHelperParam) ++
        cls.blameType.toSeq.map(t => Term.Param(Nil, q"blame", Some(t), Some(q"rewriter.dispatch(subject.blame)"))) :+
        Term.Param(List(), q"o", Some(t"Origin"), Some(q"rewriter.dispatch(subject.o)"))}): ${cls.typ}[Post] = {
        ${ColDefs.DECLARATION_NAMESPACE.foldLeft(
          cls.make(cls.params.map(p => Term.Name(p.name.value)), q"blame", q"o")
        ){
          case (e, (kind, nodes)) if nodes.contains(cls.baseName) =>
            q"rewriter.${ColDefs.scopes(kind)}.scope { $e }"
          case (e, _) => e
        }}
      }
    }
  """)

  def makeRewriteHelper(cls: ClassDef): Stat = q"""
    implicit class ${cls.rewriteHelperName}[Pre, Post](val subject: ${cls.typ}[Pre])(implicit val rewriter: AbstractRewriter[Pre, Post])
      extends ${Template(Nil, List(Init(t"${Type.Name("Rewrite" + cls.baseName + "Impl")}[Pre, Post]", Name.Anonymous(), Nil)), Self(Name.Anonymous(), None), Nil)}
  """

  def make(): List[(String, List[Stat])] = List("RewriteHelpers" -> List(q"""
    object RewriteHelpers {
      ..${info.defs.map(makeRewriteHelper).toList}
    }
  """)) ++ info.defs.map(makeRewriteHelperImpl).toList
}
