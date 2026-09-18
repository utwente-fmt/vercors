package vct.col.ast.lang.Isar

import vct.col.ast.{
  IsarDataConstructor,
  IsarDefinitionCommand,
  IsarInterpretationCommand,
  IsarLiftDefinitionCommand,
  IsarLocaleCommand,
  IsarPartialConstructorCommand,
}
import vct.col.ast.ops.IsarInterpretationCommandOps
import vct.col.print._

trait IsarInterpretationCommandImpl[G] extends IsarInterpretationCommandOps[G] {
  this: IsarInterpretationCommand[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("interpretation") <+> Text(
      this.target.decl.asInstanceOf[IsarLocaleCommand[G]].name + "_signature"
    ) <+> Doc.spread(this.localeParameters.map { f =>
      f.decl match {
        case c: IsarDataConstructor[G] => Text(c.name)
        case d: IsarDefinitionCommand[G] => Text(d.name)
        case l: IsarLiftDefinitionCommand[G] => Text(l.name)
        case lc: IsarPartialConstructorCommand[G] => Text(lc.name)
      }
    }) </> Text("apply unfold_locales") </> Text("sorry")
}
