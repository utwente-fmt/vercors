import ColDefs._

import scala.meta._

case class ColHelperRewriteBuilders(info: ColDescription) {
  def builderVar(param: Term.Param): Stat =
    q"var ${Pat.Var(Term.Name(param.name.value))}: Option[${MetaUtil.substituteTypeName("G", t"Post")(param.decltpe.get)}] = None"

  def setBuilderVar(param: Term.Param): Stat = {
    val term = Term.Name(param.name.value)
    q"def $term(replacementValue: ${MetaUtil.substituteTypeName("G", t"Post")(param.decltpe.get)}): this.type = { $term = Some(replacementValue); this }"
  }

  def builderMakeArg(param: Term.Param): Term = {
    val term = Term.Name(param.name.value)
    q"$term.getOrElse(${info.rewriteDefault(q"subject.$term", param.decltpe.get)})"
  }

  def rewriteBuilder(cls: ClassDef): Stat = q"""
    class ${cls.rewriteBuilderName}[Pre, Post](subject: ${cls.typ}[Pre])(implicit val rewriter: AbstractRewriter[Pre, Post]) {
      def build(): ${cls.typ}[Post] = {
        ${cls.make(cls.params.map(builderMakeArg), q"$BLAME_TERM.getOrElse(subject.$BLAME_TERM)", q"$ORIGIN_TERM.getOrElse(rewriter.dispatch(subject.$ORIGIN_TERM))")}
      }

      var $ORIGIN_PAT: Option[Origin] = None
      def $ORIGIN_TERM(replacementValue: Origin): this.type = { $ORIGIN_TERM = Some(replacementValue); this }

      ..${
        cls.blameType match {
          case Some(t) =>
            q"""
            var $BLAME_PAT: Option[$t] = None
            def $BLAME_TERM(replacementValue: $t): this.type = { $BLAME_TERM = Some(replacementValue); this }
            """.stats
          case None => List()
        }
      }

      ..${cls.params.map(setBuilderVar)}
      ..${cls.params.map(builderVar)}
    }
  """

  def make(): List[Stat] = q"""
    import RewriteHelpers._
    object RewriteBuilders {
      ..${info.defs.map(rewriteBuilder).toList}
    }
  """.stats
}
