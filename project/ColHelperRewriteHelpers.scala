import ColDefs._

import scala.meta._

case class ColHelperRewriteHelpers(info: ColDescription) {
  def rewriteHelperParam(param: Term.Param): Term.Param =
    Term.Param(List(),
      param.name,
      param.decltpe,
      Some(info.rewriteDefault(Term.Select(q"subject", Term.Name(param.name.value)), param.decltpe.get)))

  def makeRewriteHelper(cls: ClassDef): Stat = q"""
    implicit class ${cls.rewriteHelperName}(val subject: ${cls.typ})(implicit val rewriter: AbstractRewriter) {
      def rewrite(..${cls.params.map(rewriteHelperParam)}): ${cls.typ} = {
        ${cls.make(cls.params.map(p => Term.Name(p.name.value)), q"subject.$BLAME_TERM", q"subject.$ORIGIN_TERM")} }
    }
  """

  def make(): List[Stat] = List(q"""
    object RewriteHelpers {
      ..${info.defs.map(makeRewriteHelper).toList}
    }
  """)
}
