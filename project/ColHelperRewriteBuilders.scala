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
        ${cls.make(cls.params.map(builderMakeArg), q"blame.getOrElse(subject.blame)", q"o.getOrElse(rewriter.dispatch(subject.o))")}
      }

      var o: Option[Origin] = None
      def o(replacementValue: Origin): this.type = { o = Some(replacementValue); this }

      ..${
        cls.blameType match {
          case Some(t) =>
            q"""
            var blame: Option[$t] = None
            def blame(replacementValue: $t): this.type = { blame = Some(replacementValue); this }
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
