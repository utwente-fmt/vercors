import ColDefs._

import scala.meta._

case class ColHelperRewriteHelpers(info: ColDescription) {
  def rewriteHelperParam(param: Term.Param): Term.Param =
    Term.Param(List(),
      param.name,
      Some(Type.ByName(MetaUtil.substituteTypeName("G", Type.Name("Post"))(param.decltpe.get))),
      Some(info.rewriteDefault(Term.Select(q"subject", Term.Name(param.name.value)), param.decltpe.get)))

  def makeRewriteHelper(cls: ClassDef): Stat = q"""
    implicit class ${cls.rewriteHelperName}[Pre, Post](val subject: ${cls.typ}[Pre])(implicit val rewriter: AbstractRewriter[Pre, Post]) {
      def rewrite(..${cls.params.map(rewriteHelperParam) ++
        cls.blameType.toSeq.map(t => Term.Param(Nil, q"blame", Some(t), Some(q"subject.blame"))) :+
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
  """

  def make(): List[Stat] = List(q"""
    object RewriteHelpers {
      ..${info.defs.map(makeRewriteHelper).toList}
    }
  """)
}
